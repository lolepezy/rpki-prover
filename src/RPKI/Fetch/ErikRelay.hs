{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Fetch.ErikRelay where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens hiding (indices, Indexable)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Generics.Product.Typed

import qualified Data.List.NonEmpty          as NonEmpty

import qualified Data.ByteString                 as BS
import           Data.Text                       (Text)
import           Data.Data
import           Data.Foldable                   (for_)
import           Data.Maybe 
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map            
import qualified Data.Map.Monoidal.Strict        as MonoidalMap     
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           Time.Types

import           UnliftIO (pooledForConcurrentlyN, pooledForConcurrently)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Repository
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storage
import           RPKI.Time
import           RPKI.Parallel
import qualified RPKI.Util as U                       
import           RPKI.Rsync
import           RPKI.Fetch.Http
import           RPKI.TAL
import           RPKI.RRDP.RrdpFetch


fetchErik :: MonadIO m => AppContext s -> URI -> FQDN -> ValidatorT m ErikIndex
fetchErik AppContext {..} relayUri (FQDN fqdn) = do
    (indexBs, _, httpStatus, _ignoreEtag) <- fetchIndex
    ErikIndex {..} <- vHoist $ parseErikIndex indexBs

    partitions <- liftIO $ pooledForConcurrentlyN 4 partitionList fetchPartition    

    for_ partitions $ \case 
        (uri, Left e, vs) -> do 
            embedState vs
            logError logger [i|Failed to download partition #{uri}.|]

        (uri, Right partition, vs) -> do
            embedState vs
            liftIO $ fetchManifests partition
                    

    appError $ UnspecifiedE "options" "ErikRelay fetcher is not implemented yet"
  where 
    fetchIndex = do 
        let tmpDir = configValue $ config ^. #tmpDirectory
        let maxSize = config ^. typed @ErikConf . #maxSize
        liftIO $ downloadToBS tmpDir indexUri Nothing maxSize            

    fetchPartition ErikPartitionListEntry {..} = do 
        let partUri = objectByHashUri hash
        let tmpDir = configValue $ config ^. #tmpDirectory        

        (r, vs) <- runValidatorT (newScopes' LocationFocus partUri) $ do
            (partBs, _, partStatus, _ignoreEtag) <-
                fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                    downloadHashedBS tmpDir partUri Nothing hash size
                        (\actualHash -> 
                            Left $ ErikE $ ErikHashMismatchError { 
                                expectedHash = hash,
                                actualHash = actualHash                                            
                            })

            vHoist $ parseErikPartition partBs      

        pure (partUri, r, vs)


    fetchManifestBlobs :: ErikPartition -> IO [(Hash, URI, BS.ByteString)]
    fetchManifestBlobs ErikPartition {..} = do
        let tmpDir = configValue $ config ^. #tmpDirectory                
        pooledForConcurrentlyN 4 manifestList $ \ManifestListEntry {..} -> do
            let manUri = objectByHashUri hash
            (manBs, _, manStatus, _ignoreEtag) <- downloadToBS tmpDir manUri Nothing size
            pure (hash, manUri, manBs)        

    fetchManifests partition@ErikPartition {..} = do
        -- manifestBlobs <- fetchManifestBlobs partition

        -- for_ manifestList $ \ManifestListEntry {..} -> do
        --     let manUri = objectByHashUri hash
        --     (manBs, _, manStatus, _ignoreEtag) <- do
        --         let tmpDir = configValue $ config ^. #tmpDirectory                
        --         liftIO $ downloadToBS tmpDir manUri Nothing size
            
        --     (r, vs) <- runValidatorT (newScopes' LocationFocus manUri)
        --         $ vHoist $ parseMft manBs        

        --     case r of 
        --         Left e -> do 
        --             embedState vs
        --             logError logger [i|Failed to download manifest #{manUri}|]

        --         Right manifest -> do
        --             embedState vs
        --             -- storeManifest manifest manUri
        pure ()

    indexUri = URI [i|#{relayUri}/.well-known/erik/index/#{fqdn}|]

    objectByHashUri hash = 
        URI [i|#{relayUri}/.well-known/ni/sha-256/#{U.hashAsBase64 hash}|]
        
