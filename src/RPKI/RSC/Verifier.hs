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


rscVerify :: Storage s => AppContext s -> FilePath -> FilePath -> ValidatorT IO ()
rscVerify appContext@AppContext {..} rscFile directory = do
    bs        <- fromTry (ParseE . ParseError . fmtEx) $ BS.readFile rscFile
    parsedRsc <- vHoist $ fromEither $ first ParseE $ parseRSC bs

    db@DB {..} <- liftIO $ readTVarIO database

    validatedRsc <- roAppTx db $ \tx -> do
        taCerts <- getTACertificates tx taStore

        case getAKI parsedRsc of
            Nothing  -> appError $ ValidationE NoAKI
            Just aki -> do            
                -- validate bottom-up
                findCertificateChainToTA tx aki taCerts
                pure parsedRsc 

        -- verify the files
    verifyFiles validatedRsc directory
  where
    findCertificateChainToTA tx aki taCerts = do
        appError $ ValidationE NoAKI        
        pure ()

    getTACertificates tx taStore = 
        liftIO $ map ((^. #taCert) . snd) <$> DB.getTAs tx taStore

    verifyFiles parsedRsc directory = do         
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

    fileMap hashC = do 
        files <- fromTry (UnspecifiedE "No directory" . fmtEx) $ getDirectoryContents directory        
        liftIO $ for files $ \f -> do 
            let fullPath = directory </> f
            (f, ) . mkHash <$> runConduitRes (sourceFile fullPath .| hashC)                

    findHashFunc (DigestAlgorithmIdentifier oid) = 
        case () of 
              _ | oid == id_sha256 -> Just $ sinkHash S256.init S256.update S256.finalize            
                | oid == id_sha512 -> Just $ sinkHash S512.init S512.update S512.finalize
                | otherwise        -> Nothing

    sinkHash initialValue updateValue finalValue = do 
        loop initialValue        
      where    
        loop ctx =
            await >>= \case
                Nothing    -> pure $ finalValue ctx
                Just chunk -> loop $ updateValue ctx chunk