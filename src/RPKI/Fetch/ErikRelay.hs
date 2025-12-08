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

import           Control.Lens hiding (index, indices, Indexable)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Generics.Product.Typed
import           Data.String.Interpolate.IsString
import qualified Data.Text                       as Text

import           System.Directory
import           System.FilePath
import           UnliftIO (pooledForConcurrentlyN)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Base.Storage
import qualified RPKI.Util as U                       
import           RPKI.Fetch.Http
import           RPKI.Fetch.DirectoryTraverse
import qualified RPKI.Store.Database    as DB


fetchErik :: Storage s
            => AppContext s 
            -> WorldVersion
            -> URI 
            -> FQDN 
            -> ValidatorT IO ()
fetchErik 
    appContext@AppContext {..} 
    worldVersion 
    relayUri 
    fqdn@(FQDN fqdn_) = do

    downloadSemaphore <- newSemaphoreIO 10
    doFetch downloadSemaphore
  where 

    doFetch downloadSemaphore =
        withDir indexDir $ \_ -> do 
            ErikIndex {..} <- getIndex            
            vs <- fmap mconcat $ liftIO $ pooledForConcurrentlyN 4 partitionList $ \p -> do 
                getPartition p >>= \case 
                    (hash, Left e, vs) -> do 
                        logError logger [i|Failed to download partition #{hash}.|]
                        pure vs

                    (hash, Right partition, vs) ->                    
                        getManifests hash partition                            

            embedState vs
            -- Now traverse all downloaded objects and load them into the storage,
            -- the same way as it happens for rsynced repositories.
            loadObjectsFromFS appContext worldVersion (const Nothing) indexDir 
      where
    
        getIndex = do 
            let tmpDir = configValue $ config ^. #tmpDirectory
            let maxSize = config ^. typed @ErikConf . #maxSize
            (indexBs, _, httpStatus, _ignoreEtag) <- 
                    fromTryM (ErikE . Can'tDownloadObject . U.fmtEx) $                                      
                        downloadToBS tmpDir indexUri Nothing maxSize                    
            index <- vHoist $ parseErikIndex indexBs                
            logDebug logger [i|Downloaded Erik index #{index}, HTTP status: #{httpStatus}|]

            join $ rwTxT database $ \tx db -> do 
                DB.getErikIndex tx db relayUri fqdn >>= \case 
                    Nothing -> do 
                        DB.saveErikIndex tx db relayUri fqdn index
                        pure $ logDebug logger $ 
                                [i|No Erik index for #{fqdn_} in the database, |] <> 
                                [i|downloading from relay #{relayUri}.|]
                    Just existing 
                        | existing == index -> 
                            pure $ logDebug logger 
                                [i|Erik index for #{fqdn_} didn't change since the last synchronisation.|]
                        | otherwise -> do 
                            DB.saveErikIndex tx db relayUri fqdn index
                            pure $ logInfo logger 
                                [i|Erik index for #{fqdn_} changed, updating from relay #{relayUri}.|]              
            pure index

        getPartition ErikPartitionListEntry {..} = do 
            z <- roTxT database $ \tx db -> DB.getErikPartition tx db hash
            case z of 
                Nothing -> do     
                    logDebug logger [i|No Erik partition #{U.hashAsBase64 hash} in the database, downloading from relay #{relayUri}.|]
                    z <- fetchAndParsePartition
                    case z of 
                        (uri, Left e, vs) -> do 
                            logError logger [i|Failed to download Erik partition #{uri}|]
                            pure (hash, Left e, vs)

                        (uri, Right partition, vs) -> do
                            rwTxT database $ \tx db -> DB.saveErikPartition tx db hash partition                        
                            logDebug logger [i|Stored Erik partition #{U.hashAsBase64 hash} in the database.|]                        
                            pure (hash, Right partition, mempty)

                Just part@ErikPartition {..} -> do 
                    logDebug logger [i|Found Erik partition #{U.hashAsBase64 hash} in the database.|]
                    pure (hash, Right part, mempty)
          where
            fetchAndParsePartition = do 
                let partUri = objectByHashUri hash
                let partitionDir = indexDir </> U.firstByteStr hash
                -- It will be cleaned up by the top level
                createDirectoryIfMissing True partitionDir

                (r, vs) <- withSemaphore downloadSemaphore $ 
                    runValidatorT (newScopes' LocationFocus partUri) $ do            
                        let partitionFile = partitionDir </> show hash
                        (partBs, _, partStatus) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed partUri partitionFile hash size
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })

                        vHoist $ parseErikPartition partBs      

                pure (partUri, r, vs) 

        getManifests partitionHash partition@ErikPartition {..} = do
            let partitionDir = indexDir </> U.firstByteStr partitionHash        

            fmap mconcat $ pooledForConcurrentlyN 4 manifestList $ \mle@ManifestListEntry {..} -> do
                z <- roTxT database $ \tx db -> DB.getByHash tx db hash
                case z of 
                    Just (Located _ (MftRO mft)) -> do
                        logDebug logger [i|Manifest #{U.hashAsBase64 hash} already in the database.|]
                        getManifestChildren mft

                    Just (Located locations ro) -> do
                        logDebug logger $ [i|Manifest hash #{U.hashAsBase64 hash} points to an existing |] <> 
                                        [i|object that is not a manifest #{pickLocation locations}, |] <> 
                                        "it almost surely means broken Erik relay."
                        pure mempty
                    Nothing -> do
                        (r, vs) <- fetchAndParseManifest mle
                        case r of 
                            Left e -> do 
                                logError logger [i|Could not download/parse manifest #{U.hashAsBase64 hash}.|]
                                pure vs
                            Right mft -> do 
                                (vs <>) <$> getManifestChildren mft
          where
            fetchAndParseManifest ManifestListEntry {..} = do  
                let partitionDir = indexDir </> U.firstByteStr partitionHash
                let manifestDir = partitionDir </> U.firstByteStr hash
                createDirectoryIfMissing True manifestDir

                let manifestUri = objectByHashUri hash

                withSemaphore downloadSemaphore $ 
                    runValidatorT (newScopes' LocationFocus manifestUri) $ do                
                        let manifestFile = manifestDir </> show hash <> ".mft"
                        (manBs, _, manStatus) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed manifestUri manifestFile hash size
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })
                        
                        vHoist $ parseMft manBs


            getManifestChildren mft = do                
                let mftChildren = getMftChildren mft

                let partitionDir = indexDir </> U.firstByteStr partitionHash
                let manifestDir = partitionDir </> U.firstByteStr (getHash mft)
                let childrenDir = manifestDir </> "ch"
                createDirectoryIfMissing True childrenDir
                
                fmap mconcat $ pooledForConcurrentlyN 4 mftChildren $ \MftPair {..} -> do 
                    exists <- roTxT database $ \tx db -> DB.hashExists tx db hash
                    if exists then 
                        pure mempty 
                    else do                     
                        let maxSize = Size $ fromIntegral $ config ^. #validationConfig . #maxObjectSize
                        let childFile = childrenDir </> U.firstByteStr hash </> show hash <> "-" <> Text.unpack fileName
                        let childUri = objectByHashUri hash

                        withSemaphore downloadSemaphore $ 
                            fmap snd $ runValidatorT (newScopes' LocationFocus childUri) $ do                
                                fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                    downloadToFileHashed_ childUri childFile hash maxSize
                                        (\actualHash -> Left $ ErikE $ ErikHashMismatchError { 
                                            expectedHash = hash, .. })
                        


    indexUri = URI [i|#{relayUri}/#{fqdn_}|]

    objectByHashUri hash = 
        URI [i|#{relayUri}/.well-known/ni/sha-256/#{U.hashAsBase64 hash}|]


    indexDir = let 
        tmpDir = configValue $ config ^. #tmpDirectory
        in tmpDir </> "erik" </> U.convert fqdn_

    withDir dir f = 
        bracketVT 
            (createDirectoryIfMissing True dir) 
            -- (\_ -> liftIO $ removeDirectoryRecursive dir) 
            (\_ -> pure ()) 
            (\_ -> f dir)           
        