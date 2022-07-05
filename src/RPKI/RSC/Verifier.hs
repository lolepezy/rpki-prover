{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.RSC.Verifier where

import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class


import           Control.Lens
import           Conduit

import qualified Crypto.Hash.SHA256        as S256
import qualified Crypto.Hash.SHA512        as S512

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8

import           Data.Generics.Product.Typed
import           GHC.Generics (Generic)

import           Data.Bifunctor
import           Data.Maybe
import           Data.List
import           Data.Foldable (for_)
import           Data.Int                         (Int64)
import qualified Data.Text                        as Text
import qualified Data.Map.Strict as Map

import           Data.Tuple.Strict

import           Data.Hourglass
import           Data.String.Interpolate.IsString
import           System.Exit
import           System.Directory
import           System.FilePath                  ((</>))

import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.Parse.Parse

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time

import           RPKI.SLURM.Types
import           RPKI.Util

import qualified RPKI.Store.Database              as DB
import Data.Traversable (for)


data VerifyPath = FileList [FilePath] 
                | Directory FilePath
    deriving stock (Show, Eq, Ord, Generic)    


rscVerify :: Storage s => AppContext s -> FilePath -> VerifyPath -> ValidatorT IO ()
rscVerify appContext@AppContext {..} rscFile verifyPath = do

    db@DB {..} <- liftIO $ readTVarIO database

    -- First check that there's some validated data
    lastVersion <- liftIO $ roTx db $ getLastCompletedVersion db    
    when (isNothing lastVersion) $ appError $ ValidationE NoValidatedVersion    
    
    bs        <- fromTry (ParseE . ParseError . fmtEx) $ BS.readFile rscFile
    parsedRsc <- vHoist $ fromEither $ first ParseE $ parseRSC bs    

    validatedRsc <- roAppTx db $ \tx -> do
        taCerts <- getTACertificates tx taStore
        case getAKI parsedRsc of
            Nothing  -> appError $ ValidationE NoAKI
            Just aki -> do            
                -- validate bottom-up
                findCertificateChainToTA tx aki taCerts
                pure parsedRsc 

    verifyFiles validatedRsc
  where
    findCertificateChainToTA tx aki taCerts = do
        appError $ ValidationE NoAKI        

    getTACertificates tx taStore = 
        liftIO $ map ((^. #taCert) . snd) <$> DB.getTAs tx taStore

    verifyFiles parsedRsc = do         
        let rsc = getCMSContent $ parsedRsc ^. #cmsPayload
        let digest = rsc ^. #digestAlgorithm        
        case findHashFunc digest of 
            Nothing       -> appError $ ValidationE $ UnsupportedHashAlgorithm digest
            Just hashFunc -> do 
                actualFiles <- fileMap hashFunc
                let checkList = Map.fromList $ map (\(T2 t h) -> (h, t)) $ rsc ^. #checkList
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
                liftIO $ for files $ \f -> do 
                    let fullPath = directory </> f
                    (f, ) . mkHash <$> runConduitRes (sourceFile fullPath .| hashC)                

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

