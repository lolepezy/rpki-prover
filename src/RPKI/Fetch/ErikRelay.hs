{-# LANGUAGE OverloadedStrings  #-}

module RPKI.Fetch.ErikRelay where

import           Control.Lens hiding (index, indices, Indexable)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Error.Class
import           Data.Generics.Product.Typed
import           Data.String.Interpolate.IsString
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Set                        as Set

import           System.Directory
import           System.FilePath
import           UnliftIO (tryAny)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppMonadUtil
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
import           RPKI.Worker
import           RPKI.Time
import           RPKI.Metrics.System

data IndexFetch index = SameIndex index | UpdatedIndex index
    deriving (Show, Eq, Ord)

runErikFetchWorker :: Storage s
                    => AppContext s
                    -> FetchConfig
                    -> WorldVersion
                    -> URI
                    -> FQDN
                    -> ValidatorT IO ()
runErikFetchWorker appContext@AppContext {..} fetchConfig worldVersion relayUri fqdn@(FQDN fqdn_) = do

    -- This is for humans to read in `top` or `ps`, actual parameters
    -- are passed as 'ErikFetchParams'.
    let workerId = WorkerId [i|version:#{worldVersion}:erik-fetch:#{fqdn_}|]

    let maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount
    let arguments =
            [ show workerId ] <>
            rtsArguments [
                rtsN maxCpuAvailable,
                rtsA "4m",
                rtsAL "4m",
                "-Fd1",
                "--disable-delayed-os-memory-return",
                rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #erikWorkerMemoryMb) ]

    scopes <- askScopes
    workerInput <- makeWorkerInput appContext workerId
                        (ErikFetchParams scopes fetchConfig relayUri fqdn worldVersion)
                        (Timebox $ fetchConfig ^. #erikTimeout)
                        (Just $ asCpuTime $ fetchConfig ^. #cpuLimit)

    workerInfo <- newWorkerInfo (GenericWorker "erik-fetch") (fetchConfig ^. #erikTimeout) (U.convert $ show workerId)
    wr@WorkerResult {..} <- runWorker logger workerInput arguments workerInfo
    case payload of
        Left (ErrorResult e) ->
            appError $ InternalE $ WorkerError e
        Right (ErikFetchResult z) -> do
            logWorkerDone logger workerId wr
            pushSystem logger $ cpuMemMetric "fetch" cpuTime clockTime maxMemory
            embedValidatorT $ pure z

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

    downloadSemaphore <- newSemaphoreIO 50
    doFetch downloadSemaphore
  where 

    parallelism = fromIntegral $ config ^. typed @ErikConf . #parallelism

    doFetch downloadSemaphore =
        withDir indexDir $ \_ -> do 
            U.ifJustM getIndex $ \index@ErikIndex {..} -> do 
                logInfo logger [i|Erik index for #{fqdn_} updated, downloading partitions from relay #{relayUri}.|]
                
                when (indexScope /= fqdn_) $
                    appError $ ErikE $ ErikIndexScopeMismatch { expectedScope = fqdn, actualScope = indexScope }

                logDebug logger [i|Erik index from #{indexUri} has #{index}.|]
                void $ fmap mconcat $ concurrentlyVTLenientN parallelism partitionList $ \partitionRef@ErikPartitionRef {..} -> do
                    partition <- getPartition partitionRef                        
                    logDebug logger [i|Downloaded Erik partition #{U.hashAsBase64Url hash}: #{partition}.|]
                    getManifests indexScope hash partition

                logDebug logger [i|Finished fetching Erik relay #{indexDir} for #{fqdn_}.|]

                -- Now traverse all downloaded objects and load them into the storage,
                -- the same way it happens for rsync-ed repositories.
                loadObjectsFromFS appContext worldVersion (const Nothing) indexDir 
      where
    
        getIndex :: ValidatorT IO (Maybe ErikIndex)
        getIndex = do 
            let tmpDir = configValue $ config ^. #tmpDirectory
            let maxSize = config ^. typed @ErikConf . #maxSize
            (indexBs, _, httpStatus, _ignoreEtag) <- 
                    fromTryM (ErikE . Can'tDownloadObject . U.fmtEx) $                                      
                        downloadToBS tmpDir indexUri Nothing maxSize
            when (httpStatus /= mempty) $ do 
                appError $ ErikE $ Can'tDownloadObject [i|Could not download index #{indexUri}, http status = #{httpStatus}|]

            index <- vHoist $ parseErikIndex indexBs                
            logDebug logger [i|Downloaded Erik index for #{fqdn_}, HTTP status: #{httpStatus}|]

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

        getPartition :: ErikPartitionRef -> ValidatorT IO ErikPartition
        getPartition ErikPartitionRef {..} = do 
            z <- roTxT database $ \tx db -> DB.getErikPartition tx db hash
            case z of 
                Nothing -> do     
                    logDebug logger [i|No Erik partition #{U.hashAsBase64Url hash} in the database, downloading from relay #{relayUri}.|]
                    partition <- fetchAndParsePartition
                    rwTxT database $ \tx db -> DB.saveErikPartition tx db hash partition
                    logDebug logger [i|Stored Erik partition #{U.hashAsBase64Url hash} in the database.|]                        
                    pure partition

                Just partition -> do 
                    logDebug logger [i|Found Erik partition #{U.hashAsBase64Url hash} in the database.|]
                    pure partition
          where
            fetchAndParsePartition :: ValidatorT IO ErikPartition
            fetchAndParsePartition = do                                 
                -- It will be cleaned up by the top level
                liftIO $ createDirectoryIfMissing True (partitionDir hash)

                let partUri = objectByHashUri hash                
                let partitionFile = partitionDir hash </> "partition-" <> show hash

                logDebug logger [i|Downloading Erik partition #{U.hashAsBase64Url hash} from #{partUri} to #{partitionFile}.|]

                withSemaphoreVT downloadSemaphore $
                    vFocusOn LocationFocus partUri $ do                        
                        (partBs, _, _) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed partUri partitionFile hash size
                                    (\actualStatus -> Left $ ErikE $ Can'tDownloadObject 
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { 
                                        expectedHash = hash, .. })

                        vHoist $ parseErikPartition partBs                      

        getManifests :: Text -> Hash -> ErikPartition -> ValidatorT IO ()
        getManifests scope partitionHash ErikPartition {..} = do            
            fmap mconcat $ concurrentlyVTLenientN parallelism manifestList $ \manifestRef@ErikManifestRef {..} -> do
                -- RFC §4: all locations in a ManifestRef MUST be encompassed within indexScope
                let badLocations = filter (not . locationInScope scope) locations
                when (not $ null badLocations) $
                    appError $ ErikE $ ErikManifestOutsideScope { location = badLocations, scope = scope }
                
                z <- roTxT database $ \tx db -> DB.getByHash tx db hash
                case z of 
                    Just (Located _ (MftRO mft)) -> do
                        logDebug logger [i|Manifest #{U.hashAsBase64Url hash} already in the database.|]
                        void $ fetchManifestChildren mft

                    Just (Located objectLocations _) -> do
                        logDebug logger $ [i|Manifest hash #{U.hashAsBase64Url hash} points to an existing |] <>
                                        [i|object that is not a manifest #{pickLocation objectLocations}, |] <>
                                        "it almost surely means broken Erik relay."

                    Nothing -> do
                        mft <- fetchAndParseManifest manifestRef
                        void $ fetchManifestChildren mft

          where
            fetchAndParseManifest ErikManifestRef {..} = do
                
                liftIO $ createDirectoryIfMissing True (manifestDir hash)

                let manifestUri = objectByHashUri hash
                withSemaphoreVT downloadSemaphore $ 
                    vFocusOn LocationFocus manifestUri $ do
                        let manifestFile = manifestDir hash </> show hash <> ".mft"
                        (manifestBs, _, _) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed manifestUri manifestFile hash size
                                    (\actualStatus -> Left $ ErikE $ Can'tDownloadObject 
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })
                        
                        vHoist $ parseMft manifestBs


            fetchManifestChildren :: MftObject -> ValidatorT IO (Size, HttpStatus)
            fetchManifestChildren mft = do       
                let childrenDir_ = childrenDir $ getHash mft                         
                liftIO $ createDirectoryIfMissing True childrenDir_
                
                let mftChildren = filter (\MftPair {..} -> supportedExtensionByErik $ Text.unpack fileName) $ getMftChildren mft                
                -- logDebug logger [i|Downloading children of manifest #{U.hashAsBase64Url (getHash mft)}: #{mftChildren}.|]

                -- This is to avoid a directory with a lot of files in it                
                forM_ (Set.fromList $ map (\MftPair {..} -> U.firstByte hash) mftChildren) $ \firstByte -> do 
                    liftIO $ createDirectoryIfMissing True $ childrenDir_ </> show firstByte

                fmap mconcat $ concurrentlyVTLenientN parallelism mftChildren $ \MftPair {..} -> do 
                    exists <- roTxT database $ \tx db -> DB.hashExists tx db hash
                    if exists then 
                        pure mempty 
                    else do                                             
                        let childFile = childrenDir_ </> show (U.firstByte hash) </> show hash <> "-" <> Text.unpack fileName
                        let childUri = objectByHashUri hash                        
                        let maxSize = Size $ fromIntegral $ config ^. #validationConfig . #maxObjectSize                        

                        withSemaphoreVT downloadSemaphore $                                      
                            vFocusOn LocationFocus childUri $ do                            
                                let fetch = 
                                        fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                            downloadToFileHashed_ childUri childFile hash maxSize
                                                (\actualStatus -> Left $ ErikE $ Can'tDownloadObject 
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                                (\actualHash -> Left $ ErikE $ ErikHashMismatchError { 
                                                    expectedHash = hash, .. })

                                fetch `catchError` (\e -> do 
                                    logError logger [i|Could not download/parse manifest child #{U.hashAsBase64Url hash} from #{childUri}, error: #{e}.|]
                                    void $ liftIO $ tryAny $ removeFile childFile
                                    throwError e)
              
            manifestDir mftHash = partitionDir partitionHash </> "m_" <> show (U.firstByte mftHash)
            childrenDir mftHash = manifestDir mftHash </> "ch"


    indexUri = URI [i|#{relayUri}/.well-known/erik/index/#{fqdn_}|]

    objectByHashUri hash = let 
        niHash = U.hashAsBase64Url hash
        in URI [i|#{relayUri}/.well-known/ni/sha-256/#{niHash}|]

    locationInScope scope (URI uri) = U.getHostname uri == Just scope

    indexDir = let 
        tmpDir = configValue $ config ^. #tmpDirectory
        in tmpDir </> "erik" </> U.convert fqdn_

    partitionDir partitionHash = indexDir </> "p_" <> show (U.firstByte partitionHash)  

    withDir dir f = 
        bracketVT 
            (createDirectoryIfMissing True dir) 
            (\_ -> liftIO $ removeDirectoryRecursive dir) 
            -- (\_ -> pure ()) 
            (\_ -> f dir)           
        