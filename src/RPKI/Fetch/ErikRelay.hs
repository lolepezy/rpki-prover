{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

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
import           Data.Text                       (Text, index)
import           Data.Data
import           Data.Foldable                   (for_)
import           Data.Maybe 
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map            
import qualified Data.Map.Monoidal.Strict        as MonoidalMap     
import           Data.String.Interpolate.IsString

import           System.Directory
import           System.FilePath
import           UnliftIO (pooledForConcurrentlyN, pooledForConcurrently, bracket)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Repository
import           RPKI.Store.Base.Storage
import qualified RPKI.Util as U                       
import           RPKI.Fetch.Http
import qualified RPKI.Store.Database    as DB
import Data.ByteArray (create)

fetchErik :: Storage s
            => AppContext s 
            -> URI 
            -> FQDN 
            -> ValidatorT IO ()
fetchErik AppContext {..} relayUri fqdn@(FQDN fqdn_) = do
            
    withDir indexDir $ \_ -> do 
        ErikIndex {..} <- getIndex
        partitions <- liftIO $ pooledForConcurrentlyN 4 partitionList getPartition   

        for_ partitions $ \case 
            (hash, Left e, vs) -> do 
                embedState vs
                logError logger [i|Failed to download partition #{hash}.|]

            (hash, Right partition, vs) -> do
                embedState vs
                liftIO $ getManifests hash partition    

    pure ()

  where 

    getIndex :: ValidatorT IO ErikIndex
    getIndex = do 
        z <- roTxT database $ \tx db -> DB.getErikIndex tx db fqdn
        case z of 
            Nothing -> do     
                logDebug logger [i|No Erik index for #{fqdn_} in the database, downloading from relay #{relayUri}.|]
                let tmpDir = configValue $ config ^. #tmpDirectory
                let maxSize = config ^. typed @ErikConf . #maxSize
                (indexBs, _, httpStatus, _ignoreEtag) <- 
                        fromTryM (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                            downloadToBS tmpDir indexUri Nothing maxSize
                vHoist $ parseErikIndex indexBs

            Just index -> do 
                logDebug logger [i|Found Erik index for #{fqdn_} in the database.|]
                pure index

    getPartition partitionEntry@ErikPartitionListEntry {..} = do 
        z <- roTxT database $ \tx db -> DB.getErikPartition tx db hash
        case z of 
            Nothing -> do     
                logDebug logger [i|No Erik partition #{U.hashAsBase64 hash} in the database, downloading from relay #{relayUri}.|]
                z <- fetchPartition
                case z of 
                    (uri, Left e, vs) -> do 
                        logError logger [i|Failed to download Erik partition #{uri}|]
                        pure (hash, Left e, vs)

                    (uri, Right partition, vs) -> do          
                        -- Move it to a separate thread?              
                        rwTxT database $ \tx db -> DB.saveErikPartition tx db fqdn hash partition                        
                        logDebug logger [i|Stored Erik partition #{U.hashAsBase64 hash} in the database.|]                        
                        pure (hash, Right partition, mempty)

            Just part@ErikPartition {..} -> do 
                logDebug logger [i|Found Erik partition #{U.hashAsBase64 hash} in the database.|]
                pure (hash, Right part, mempty)
      where
        fetchPartition = do 
            let partUri = objectByHashUri hash
            let partitionDir = indexDir </> U.firstByteStr hash
            -- It will be cleaned up by the top level
            createDirectoryIfMissing True partitionDir

            (r, vs) <- runValidatorT (newScopes' LocationFocus partUri) $ do            
                let partitionFile = partitionDir </> show hash
                (partBs, _, partStatus) <-
                    fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                        downloadToFileHashed partUri partitionFile hash size
                            (\actualHash -> 
                                Left $ ErikE $ ErikHashMismatchError { 
                                    expectedHash = hash,
                                    actualHash = actualHash                                            
                                })

                vHoist $ parseErikPartition partBs      

            pure (partUri, r, vs)


    -- fetchManifestBlobs :: ErikPartition -> IO [(Hash, URI, BS.ByteString)]
    -- fetchManifestBlobs ErikPartition {..} = do
    --     let tmpDir = configValue $ config ^. #tmpDirectory                
    --     pooledForConcurrentlyN 4 manifestList $ \ManifestListEntry {..} -> do
    --         let manUri = objectByHashUri hash
    --         (manBs, _, manStatus, _ignoreEtag) <- downloadToBS tmpDir manUri Nothing size
    --         pure (hash, manUri, manBs)        

    getManifests partitionHash partition@ErikPartition {..} = do
        let partitionDir = indexDir </> U.firstByteStr partitionHash
        -- manifestBlobs <- fetchManifestBlobs partition

        for_ manifestList $ \mle@ManifestListEntry {..} -> do
            z <- roTxT database $ \tx db -> DB.getByHash tx db hash
            case z of 
                Just (Located _ (MftRO mft)) -> 
                    logDebug logger [i|Manifest #{U.hashAsBase64 hash} already in the database.|]

                Just (Located locations ro) -> 
                    logDebug logger $ [i|Manifest hash #{U.hashAsBase64 hash} points to an existing |] <> 
                                      [i|object that is not a manifest #{pickLocation locations}, |] <> 
                                      "that almost surely means broken Erik relay."
                Nothing -> do
                    (r, vs) <- fetchAndParseManifest mle
                    pure ()
      where
        fetchAndParseManifest ManifestListEntry {..} = do  
            let partitionDir = indexDir </> U.firstByteStr partitionHash
            let manifestDir = partitionDir </> U.firstByteStr hash
            createDirectoryIfMissing True manifestDir

            let manifestUri = objectByHashUri hash

            runValidatorT (newScopes' LocationFocus manifestUri) $ do                                
                let manifestFile = manifestDir </> show hash
                (manBs, _, manStatus) <-
                    fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                        downloadToFileHashed manifestUri manifestFile hash size
                            (\actualHash -> 
                                Left $ ErikE $ ErikHashMismatchError { 
                                    expectedHash = hash,
                                    actualHash = actualHash                                            
                                })
                
                vHoist $ parseMft manBs


    indexUri = URI [i|#{relayUri}/.well-known/erik/index/#{fqdn_}|]

    objectByHashUri hash = 
        URI [i|#{relayUri}/.well-known/ni/sha-256/#{U.hashAsBase64 hash}|]


    indexDir = let 
        tmpDir = configValue $ config ^. #tmpDirectory
        in tmpDir </> "erik" </> U.convert fqdn_

    withDir dir f = 
        bracketVT 
            (createDirectoryIfMissing True dir) 
            (\_ -> liftIO $ removeDirectoryRecursive dir) 
            (\_ -> f dir)           
        

        
    