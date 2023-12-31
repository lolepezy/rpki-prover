{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.RSC.Verifier where



import           Control.Concurrent.STM

import           Control.Monad
import           Control.Monad.IO.Class


import           Control.Lens
import           Conduit

import qualified Crypto.Hash.SHA256        as S256
import qualified Crypto.Hash.SHA512        as S512

import qualified Data.ByteString as BS

import           GHC.Generics (Generic)

import           Data.Maybe
import           Data.Foldable (for_)
import qualified Data.Text                        as Text
import qualified Data.Map.Strict as Map

import           Data.Tuple.Strict

import           Data.String.Interpolate.IsString
import           System.Directory
import           System.FilePath                  ((</>))

import           RPKI.AppMonad

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Store.Database
import           RPKI.Parse.Parse

import           RPKI.AppContext
import           RPKI.Store.Base.Storage
import           RPKI.Time

import           RPKI.Util
import           RPKI.Validation.BottomUp

import Data.Traversable (for)


data VerifyPath = FileList [FilePath] 
                | Directory FilePath
    deriving stock (Show, Eq, Ord, Generic)    


rscVerify :: Storage s => AppContext s -> FilePath -> VerifyPath -> ValidatorT IO ()
rscVerify appContext@AppContext {..} rscFile verifyPath = do

    db <- liftIO $ readTVarIO database

    -- First check that there's some validated data
    lastVersion <- liftIO $ roTx db $ getLastValidationVersion db    
    when (isNothing lastVersion) $ appError $ ValidationE NoValidatedVersion    
    
    bs        <- fromTry (ParseE . ParseError . fmtEx) $ BS.readFile rscFile
    parsedRsc <- vHoist $ parseRsc bs    

    now <- thisInstant
    void $ validateBottomUp appContext (RscRO parsedRsc) now

    verifyFiles parsedRsc
  where    
    verifyFiles parsedRsc = do         
        let rsc = getCMSContent $ parsedRsc ^. #cmsPayload
        let digest = rsc ^. #digestAlgorithm
        case findHashFunc digest of 
            Nothing       -> appError $ ValidationE $ UnsupportedHashAlgorithm $ fmtGen digest
            Just hashFunc -> do 
                actualFiles <- fileMap hashFunc
                let checkList = Map.fromList $ map (\(T2 t h) -> (h, t)) $ rsc ^. #checkList
                let checklistText = Text.intercalate "\n" 
                        $ map (\(T2 t h) ->  Text.pack (show h) <> "\t\t" <> fromMaybe "" t) 
                        $ rsc ^. #checkList
                logDebug logger [i|Check list:
#{checklistText}|]
                for_ actualFiles $ \(Text.pack -> f, h) -> do
                    case Map.lookup h checkList of 
                        Nothing         -> appError $ ValidationE $ NotFoundOnChecklist h f
                        Just (Just f')  
                            | f /= f'   -> appError $ ValidationE $ ChecklistFileNameMismatch h f f'
                            | otherwise -> pure ()
                        _ -> pure ()

    fileMap hashC = 
        case verifyPath of 
            FileList files -> 
                liftIO $ for files $ \f -> 
                    (f, ) . mkHash <$> runConduitRes (sourceFile f .| hashC)                                                
            Directory directory -> do 
                files <- fromTry (UnspecifiedE [i|No directory #{directory}|] . fmtEx) $ getDirectoryContents directory

                liftIO $ do 
                    fmap mconcat $ for files $ \f -> do 
                        let fullPath = directory </> f
                        isFile <- doesFileExist fullPath
                        if isFile then do
                            hash <- mkHash <$> runConduitRes (sourceFile fullPath .| hashC)                            
                            pure [(f, hash)] 
                        else 
                            pure []

                    

    findHashFunc (DigestAlgorithmIdentifier oid) = 
        case () of 
              _ | oid == id_sha256 -> Just $ sinkHash S256.init S256.update S256.finalize            
                | oid == id_sha512 -> Just $ sinkHash S512.init S512.update S512.finalize
                -- TODO Support more hashes
                | otherwise        -> Nothing

    -- Conduit sink computing a hash in an incremental fashion
    sinkHash initialValue updateValue finalValue = do 
        loop initialValue        
      where    
        loop ctx =
            await >>= \case
                Nothing    -> pure $ finalValue ctx
                Just chunk -> loop $ updateValue ctx chunk        
            

