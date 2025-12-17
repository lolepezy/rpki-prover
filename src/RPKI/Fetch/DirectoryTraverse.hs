{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Fetch.DirectoryTraverse where

import           Data.Generics.Product.Typed

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens hiding (indices, Indexable)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class

import qualified Data.List.NonEmpty          as NonEmpty
import qualified Data.ByteString                  as BS

import           Data.Bifunctor
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate.IsString

import           GHC.Generics

import qualified Streaming.Prelude                as S
import           System.FilePath
import           System.Directory                 (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import           System.IO

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Parallel

import           RPKI.Store.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database              as DB
import qualified RPKI.Util                        as U
import Control.Concurrent.STM (readTVar)


-- | Recursively traverse given directory and save all the parseable 
-- | objects into the storage.
-- 
-- | Is not supposed to throw exceptions.
loadObjectsFromFS :: forall s . Storage s =>                         
                        AppContext s 
                    -> WorldVersion 
                    -> (FilePath -> Maybe RsyncURL) 
                    -> FilePath 
                    -> ValidatorT IO ()
loadObjectsFromFS AppContext{..} worldVersion restoreUrl rootPath = do
    db <- liftIO $ readTVarIO database
    doLoad db
  where        
    doLoad db = 
        txFoldPipeline 
            (2 * cpuParallelism)
            traverseFS
            (DB.rwAppTx db)
            saveStorable   
      where
        cpuParallelism = config ^. typed @Parallelism . #cpuParallelism

        traverseFS = 
            mapException (AppException . RsyncE . FileReadError . U.fmtEx) <$> 
                traverseDirectory rootPath

        traverseDirectory currentPath = do
            names <- liftIO $ getDirectoryContents currentPath
            let properNames = filter (`notElem` [".", ".."]) names
            forM_ properNames $ \name -> do
                let path = currentPath </> name
                liftIO (doesDirectoryExist path) >>= \case
                    True  -> traverseDirectory path
                    False -> 
                        when (supportedExtension name) $ do         
                            let !uri = restoreUrl path
                            s <- askScopes                     
                            let task = runValidatorT s $ liftIO $ readAndParseObject path (RsyncU <$> uri)
                            a <- liftIO $ async $ evaluate =<< task
                            S.yield (a, uri)
          where
            readAndParseObject filePath rpkiURL = 
                liftIO (getSizeAndContent (config ^. typed) filePath) >>= \case                    
                    Left e          -> pure $! CantReadFile rpkiURL filePath $ VErr e
                    Right (_, blob) ->                     
                        case nameObjectType filePath of 
                            Just type_ -> do 
                                -- Check if the object is already in the storage
                                -- before parsing ASN1 and serialising it.
                                let hash = U.sha256s blob  
                                exists <- liftIO $ roTx db $ \tx -> DB.hashExists tx db hash
                                if exists 
                                    then pure $! HashExists rpkiURL hash
                                    else tryToParse hash blob type_                                    
                            Nothing -> 
                                pure $! UknownObjectType rpkiURL filePath

              where
                tryToParse hash blob type_ = do            
                    let scopes = case rpkiURL of 
                            Just u  -> newScopes' LocationFocus $ getURL u
                            Nothing -> newScopes' HashFocus hash

                    z <- runValidatorT scopes $ vHoist $ readObjectOfType type_ blob
                    (evaluate $! 
                        case z of 
                            (Left e, _) -> 
                                ObjectParsingProblem rpkiURL (VErr e) 
                                    (ObjectOriginal blob) hash
                                    (ObjectMeta worldVersion type_)                        
                            (Right ro, _) ->                                     
                                SuccessParsed rpkiURL (toStorableObject ro) type_                    
                        ) `catch` 
                        (\(e :: SomeException) -> 
                            pure $! ObjectParsingProblem rpkiURL (VErr $ RsyncE $ RsyncFailedToParseObject $ U.fmtEx e) 
                                    (ObjectOriginal blob) hash
                                    (ObjectMeta worldVersion type_)
                        )

        saveStorable tx (a, _) = do 
            (r, vs) <- fromTry (UnspecifiedE "Something bad happened in loadObjectsFromFS" . U.fmtEx) $ wait a                
            embedState vs
            case r of 
                Left e  -> appWarn e
                Right z -> case z of 
                    HashExists rpkiURL hash ->
                        forM_ rpkiURL $ \u -> DB.linkObjectToUrl tx db u hash

                    CantReadFile rpkiUrl filePath (VErr e) -> do                    
                        logError logger [i|Cannot read file #{filePath}, error #{e} |]                    
                        case rpkiUrl of 
                            Just u  -> inSubLocationScope (getURL u) $ appWarn e
                            Nothing -> vFocusOn TextFocus (U.convert filePath) $ appWarn e

                    UknownObjectType rpkiUrl filePath -> do 
                        logError logger [i|Unknown object type: url = #{rpkiUrl}, path = #{filePath}.|]
                        let complain = appWarn $ RsyncE $ RsyncUnsupportedObjectType $ U.convert filePath
                        case rpkiUrl of 
                            Just u  -> inSubLocationScope (getURL u) complain
                            Nothing -> vFocusOn TextFocus (U.convert filePath) complain

                    ObjectParsingProblem rpkiUrl (VErr e) original hash objectMeta -> do
                        logError logger [i|Couldn't parse object #{rpkiUrl}, error #{e}, will cache the original object.|]   
                        case rpkiUrl of 
                            Just u  -> inSubLocationScope (getURL u) $ appWarn e
                            Nothing -> vFocusOn HashFocus hash $ appWarn e
                        DB.saveOriginal tx db original hash objectMeta
                        forM_ rpkiUrl $ \u -> DB.linkObjectToUrl tx db u hash                                  

                    SuccessParsed rpkiUrl so@StorableObject {..} type_ -> do 
                        DB.saveObject tx db so worldVersion                    
                        forM_ rpkiUrl $ \u -> DB.linkObjectToUrl tx db u (getHash object)
                        updateMetric @RsyncMetric @_ (#processed %~ Map.unionWith (+) (Map.singleton (Just type_) 1))

                    other -> 
                        logDebug logger [i|Weird thing happened in `saveStorable` #{other}.|]                          
                  

getSizeAndContent :: ValidationConfig -> FilePath -> IO (Either AppError (Integer, BS.ByteString))
getSizeAndContent vc path = do 
    r <- first (RsyncE . FileReadError . U.fmtEx) <$> readSizeAndContet
    pure $ r >>= \case 
                (_, Left e)  -> Left e
                (s, Right b) -> Right (s, b)    
  where    
    readSizeAndContet = try $ do
        withFile path ReadMode $ \h -> do
            size <- hFileSize h
            case validateSize vc size of
                Left e  -> pure (size, Left $ ValidationE e)
                Right _ -> do
                    r <- BS.hGetContents h                                
                    pure (size, Right r)

validateSize :: ValidationConfig -> Integer -> Either ValidationError Integer
validateSize vc s =    
    case () of
        _
            | s < vc ^. #minObjectSize -> Left $ ObjectIsTooSmall s
            | s > vc ^. #maxObjectSize -> Left $ ObjectIsTooBig s
            | otherwise                -> pure s

data ObjectProcessingResult =           
          CantReadFile (Maybe RpkiURL) FilePath VIssue
        | HashExists (Maybe RpkiURL) Hash
        | UknownObjectType (Maybe RpkiURL) String
        | ObjectParsingProblem (Maybe RpkiURL) VIssue ObjectOriginal Hash ObjectMeta
        | SuccessParsed (Maybe RpkiURL) (StorableObject RpkiObject) RpkiObjectType
    deriving stock (Show, Eq, Generic)