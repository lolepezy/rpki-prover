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

import Conduit

import           Control.Lens

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


rscVerify :: Storage s => AppContext s -> FilePath -> FilePath -> ValidatorT IO ()
rscVerify appContext@AppContext {..} rscFile directory = do
    bs     <- fromTry (ParseE . ParseError . fmtEx) $ BS.readFile rscFile
    parsedRsc <- vHoist $ fromEither $ first ParseE $ parseRSC bs

    db@DB {..} <- liftIO $ readTVarIO database
    roAppTx db $ \tx -> do
        taCerts <- getTACertificates tx taStore

        case getAKI parsedRsc of
            Nothing  -> appError $ ValidationE NoAKI
            Just aki -> do            
                -- validate bottom-up
                findCertificateChainToTA tx aki taCerts
                -- verify the files
                verifyFiles parsedRsc directory
  where
    findCertificateChainToTA tx aki taCerts = do
        appError $ ValidationE NoAKI        
        pure ()

    getTACertificates tx taStore = 
        liftIO $ map ((^. #taCert) . snd) <$> DB.getTAs tx taStore

    verifyFiles parsedRsc directory = do         
        let digest = getCMSContent (parsedRsc ^. #cmsPayload) ^. #digestAlgorithm        
        case hashFunc digest of 
            Nothing -> appError $ ValidationE $ UnsupportedHashAlgorithm digest
            Just hashC -> do 
                files <- fromTry (UnspecifiedE "No directory" . fmtEx) $ getDirectoryContents directory
                for_ files $ \f -> do 
                    h <- mkHash <$> liftIO (runConduitRes $ sourceFile f .| hashC)
                    pure ()            
        pure ()

    hashFunc (DigestAlgorithmIdentifier oid) = 
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