{-# LANGUAGE OverloadedStrings  #-}

module RPKI.Fetch.ErikRelay where

import           Control.Lens hiding (index, indices, Indexable)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Generics.Product.Typed
import           Data.String.Interpolate.IsString
import qualified Data.Text                       as Text
import qualified Data.Set                        as Set

import           System.Directory
import           System.FilePath
import           UnliftIO (pooledForConcurrentlyN, tryAny)

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

data IndexFetch index = SameIndex index | UpdatedIndex index
    deriving (Show, Eq, Ord)

{- 
    Implementation of the Erik relay fetcher.
    https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-erik-protocol/    
-}
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

    downloadSemaphore <- newSemaphoreIO 20
    doFetch downloadSemaphore
  where 

    concurrentlyN = pooledForConcurrentlyN 4

    doFetch downloadSemaphore =
        withDir indexDir $ \_ -> do 
            U.ifJustM getIndex $ \index@ErikIndex {..} -> do 
                logInfo logger [i|Erik index for #{fqdn_} updated, downloading partitions from relay #{relayUri}.|]
                
                when (indexScope /= fqdn_) $
                    appError $ ErikE $ ErikIndexScopeMismatch { expectedScope = fqdn, actualScope = indexScope }

                logDebug logger [i|Erik index from #{indexUri} has #{index}.|]
                vs <- fmap mconcat $ liftIO $ concurrentlyN partitionList $ \partitionRef -> do 
                    getPartition partitionRef >>= \case
                        (hash, Left e, vs) -> do 
                            logError logger [i|Failed to download partition #{hash}, error: #{e}.|]
                            pure vs

                        (hash, Right partition, vs) -> do 
                            logDebug logger [i|Downloaded Erik partition #{U.hashAsBase64Url hash}: #{partition}.|]
                            (vs <>) <$> getManifests hash partition                            

                embedState vs

                logDebug logger [i|Finished fetching Erik relay #{indexDir} for #{fqdn_}.|]

                -- Now traverse all downloaded objects and load them into the storage,
                -- the same way it happens for rsynced repositories.
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
                        pure $ do 
                            logInfo logger [i|No Erik index for #{fqdn_} in the database, downloading from relay #{relayUri}.|]
                            pure $ Just index

                    Just existing 
                        | existing == index -> 
                            pure $ do 
                                logInfo logger [i|Erik index for #{fqdn_} didn't change since the last synchronisation.|]
                                pure Nothing
                        | otherwise -> do 
                            DB.saveErikIndex tx db relayUri fqdn index
                            pure $ do 
                                logInfo logger [i|Erik index for #{fqdn_} changed, updating from relay #{relayUri}.|]              
                                pure $ Just index            

        getPartition ErikPartitionRef {..} = do 
            z <- roTxT database $ \tx db -> DB.getErikPartition tx db hash
            case z of 
                Nothing -> do     
                    logDebug logger [i|No Erik partition #{U.hashAsBase64Url hash} in the database, downloading from relay #{relayUri}.|]
                    fetchAndParsePartition >>= \case                    
                        (uri, Left e, vs) -> do 
                            logError logger [i|Failed to download Erik partition #{uri}|]
                            pure (hash, Left e, vs)

                        (uri, Right partition, vs) -> do
                            rwTxT database $ \tx db -> DB.saveErikPartition tx db hash partition
                            logDebug logger [i|Stored Erik partition #{U.hashAsBase64Url hash} in the database.|]                        
                            pure (hash, Right partition, vs)

                Just partition -> do 
                    logDebug logger [i|Found Erik partition #{U.hashAsBase64Url hash} in the database.|]
                    pure (hash, Right partition, mempty)
          where
            fetchAndParsePartition = do 
                let partUri = objectByHashUri hash                
                -- It will be cleaned up by the top level
                createDirectoryIfMissing True (partitionDir hash)

                (r, vs) <- withSemaphore downloadSemaphore $ 
                    runValidatorT (newScopes' LocationFocus partUri) $ do            
                        let partitionFile = partitionDir hash </> "partition-" <> show hash
                        (partBs, _, partStatus) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed partUri partitionFile hash size
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })

                        vHoist $ parseErikPartition partBs      

                pure (partUri, r, vs) 

        getManifests partitionHash partition@ErikPartition {..} = do            
            fmap mconcat $ concurrentlyN manifestList $ \mle@ErikManifestRef {..} -> do

                z <- roTxT database $ \tx db -> DB.getByHash tx db hash
                case z of 
                    Just (Located _ (MftRO mft)) -> do
                        logDebug logger [i|Manifest #{U.hashAsBase64Url hash} already in the database.|]
                        getManifestChildren mft

                    Just (Located locations ro) -> do
                        logDebug logger $ [i|Manifest hash #{U.hashAsBase64Url hash} points to an existing |] <> 
                                        [i|object that is not a manifest #{pickLocation locations}, |] <> 
                                        "it almost surely means broken Erik relay."
                        pure mempty
                    Nothing -> do
                        (r, vs) <- fetchAndParseManifest mle
                        case r of 
                            Left e -> do 
                                logError logger [i|Could not download or parse manifest #{U.hashAsBase64Url hash}: #{e}.|]
                                pure vs
                            Right mft -> do 
                                (vs <>) <$> getManifestChildren mft
          where
            fetchAndParseManifest ErikManifestRef {..} = do
                
                createDirectoryIfMissing True (manifestDir hash)

                let manifestUri = objectByHashUri hash
                withSemaphore downloadSemaphore $ 
                    runValidatorT (newScopes' LocationFocus manifestUri) $ do                
                        let manifestFile = manifestDir hash </> show hash <> ".mft"
                        (manifestBs, _, manStatus) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed manifestUri manifestFile hash size
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })
                        
                        vHoist $ parseMft manifestBs


            getManifestChildren mft = do       
                let childrenDir_ = childrenDir $ getHash mft                         
                createDirectoryIfMissing True childrenDir_
                
                let mftChildren = filter (\MftPair {..} -> supportedExtensionByErik $ Text.unpack fileName) $ getMftChildren mft                
                -- logDebug logger [i|Downloading children of manifest #{U.hashAsBase64Url (getHash mft)}: #{mftChildren}.|]

                -- This is to avoid a dierectory with a lot of files in it                
                forM_ (Set.fromList $ map (\MftPair {..} -> U.firstByte hash) mftChildren) $ \firstByte -> do 
                    createDirectoryIfMissing True $ childrenDir_ </> show firstByte

                fmap mconcat $ pooledForConcurrentlyN 4 mftChildren $ \MftPair {..} -> do 
                    exists <- roTxT database $ \tx db -> DB.hashExists tx db hash
                    if exists then 
                        pure mempty 
                    else do                                             
                        let childFile = childrenDir_ </> show (U.firstByte hash) </> show hash <> "-" <> Text.unpack fileName
                        let childUri = objectByHashUri hash                        
                        let maxSize = Size $ fromIntegral $ config ^. #validationConfig . #maxObjectSize                        

                        z <- withSemaphore downloadSemaphore $ do 
                            (r, vs) <- runValidatorT (newScopes' LocationFocus childUri) $ do                
                                fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                    downloadToFileHashed_ childUri childFile hash maxSize
                                        (\actualStatus -> Left $ ErikE $ Can'tDownloadObject $ U.convert $ "Http status: " <> show actualStatus)
                                        (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })
                            case r of 
                                Left e -> do 
                                    logError logger [i|Could not download/parse manifest child #{U.hashAsBase64Url hash} from #{childUri}, error: #{e}.|]
                                    void $ tryAny $ removeFile childFile
                                    pure vs
                                Right _ -> do 
                                    logDebug logger [i|Downloaded manifest child #{U.hashAsBase64Url hash} from #{childUri} to #{childFile}.|]                                            
                                    pure vs                        

                        pure z
              
            manifestDir mftHash = partitionDir partitionHash </> "m_" <> show (U.firstByte mftHash)
            childrenDir mftHash = manifestDir mftHash </> "ch"


    indexUri = URI [i|#{relayUri}/.well-known/erik/index/#{fqdn_}|]

    objectByHashUri hash = let 
        niHash = U.hashAsBase64Url hash
        in URI [i|#{relayUri}/.well-known/ni/sha-256/#{niHash}|]

    indexDir = let 
        tmpDir = configValue $ config ^. #tmpDirectory
        in tmpDir </> "erik" </> U.convert fqdn_

    partitionDir partitionHash = indexDir </> "p_" <> show (U.firstByte partitionHash)  

    withDir dir f = 
        bracketVT 
            (createDirectoryIfMissing True dir) 
            -- (\_ -> liftIO $ removeDirectoryRecursive dir) 
            (\_ -> pure ()) 
            (\_ -> f dir)           
        