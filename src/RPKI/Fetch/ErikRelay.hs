{-# LANGUAGE OverloadedStrings  #-}

module RPKI.Fetch.ErikRelay where

import           Effectful.Concurrent (Concurrent)
import           Effectful
import           GHC.Conc                         (getNumCapabilities, setNumCapabilities)
import           Effectful.Error.Static           (catchError, rethrowError)
import           Control.Lens hiding (index, indices, Indexable)
import           Control.Monad
import           Data.Generics.Product.Typed
import           Data.Proxy
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
import           RPKI.Repository
import           RPKI.Fetch.RelayPool
import qualified RPKI.Util as U                       
import           RPKI.Fetch.Http
import           RPKI.Fetch.DirectoryTraverse
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.Worker
import           RPKI.Time
import           RPKI.Metrics.System

data IndexFetch index = SameIndex index | UpdatedIndex index
    deriving (Show, Eq, Ord)

runErikFetchWorker :: ValidatorIO es => AppContext s
                    -> FetchConfig
                    -> WorldVersion
                    -> [URI]
                    -> FQDN
                    -> Eff es ErikFetchStat
runErikFetchWorker appContext@AppContext {..} fetchConfig worldVersion relayUris fqdn@(FQDN fqdn_) = do

    -- This is for humans to read in `top` or `ps`, actual parameters
    -- are passed as 'ErikFetchParams'.
    let workerId = WorkerId [i|version:#{worldVersion}:erik-fetch:#{fqdn_}|]

    -- Start single-threaded. There are a lot of Erik workers alive at once and
    -- the RTS allocates a nursery per capability, so a worker that turns out to
    -- have nothing to download never pays for more than one. `fetchErik` raises
    -- this with 'setNumCapabilities' once the index shows work worth
    -- parallelising.
    let arguments =
            [ show workerId ] <>
            rtsArguments [
                rtsN 1,
                rtsA "4m",
                rtsAL "4m",
                "-Fd1",
                "--disable-delayed-os-memory-return",
                rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #erikWorkerMemoryMb) ]

    scopes <- askScopes
    workerInput <- makeWorkerInput appContext workerId
                        (ErikFetchParams scopes fetchConfig relayUris fqdn worldVersion)
                        (Timebox $ fetchConfig ^. #erikTimeout)
                        (Just $ asCpuTime $ fetchConfig ^. #cpuLimit)

    workerInfo <- newWorkerInfo (GenericWorker "erik-fetch") (fetchConfig ^. #erikTimeout) (U.convert $ show workerId)
    wr@WorkerResult {..} <- runWorker logger workerInput arguments workerInfo
    case payload of
        Left (ErrorResult e) ->
            appError $ InternalE $ WorkerError e
        Right (ErikFetchResult z) -> do
            logWorkerDone logger workerId wr
            pushSystem logger $ cpuMemMetric "erik-fetch" cpuTime clockTime maxRtsHeap maxProcessRss
            embedValidatorT $ pure z

{- 
    Implementation of the Erik relay fetcher.
    https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-erik-protocol/    
-}
fetchErik :: (ValidatorIO es, Concurrent :> es) => AppContext s 
            -> WorldVersion
            -> [URI]
            -> FQDN 
            -> Eff es ErikFetchStat
fetchErik 
    appContext@AppContext {..} 
    worldVersion 
    relayUris 
    fqdn@(FQDN fqdn_) = do

    -- Every relay query goes through this pool: it spreads queries over the
    -- relays, falls back to the next one on failure, and enforces the
    -- per-relay and global parallelism caps.
    pool <- newRelayPool
                (fromIntegral $ config ^. typed @ErikConf . #downloadParallelism)
                (fromIntegral $ config ^. typed @ErikConf . #relayParallelism)
                relayUris
    -- Same metric rsync fills in: both end up loading a directory tree through
    -- `loadObjectsFromFS`, which counts the objects into `processed`. Timing it
    -- here is what gives that metric its `totalTimeMs`.
    timedMetric (Proxy :: Proxy TraverseMetric) $ doFetch pool
    -- Whatever happened inside -- a full download, an unchanged index, or a
    -- failure part-way through -- the pool knows which relays answered.
    toFetchStat <$> relayStats pool
  where 

    -- Only the relays that actually took part: the pool knows about every
    -- configured relay, and listing the ones that were never touched says
    -- nothing about where the objects came from.
    toFetchStat stats = ErikFetchStat 
        [ ErikRelayUsage statRelay statServed statFailed 
        | RelayStat {..} <- stats
        , statServed > 0 || statFailed > 0 ]


    parallelism = fromIntegral $ config ^. typed @ErikConf . #parallelism

    -- Raise the capability count from the 1 the worker started with, but only
    -- when there is enough independent work to use it.
    scaleUpCapabilities partitionCount = do
        let maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount
        let wanted = max 1 $ min maxCpuAvailable partitionCount
        current <- liftIO getNumCapabilities
        when (wanted > current) $ do
            liftIO $ setNumCapabilities wanted
            logDebug logger
                [i|Erik worker for #{fqdn_}: #{partitionCount} partition(s), raising -N from #{current} to #{wanted}.|]

    doFetch pool =
        withDir indexDir $ \_ -> do 
            U.ifJustM getIndex $ \index@ErikIndex {..} -> do 
                logInfo logger [i|Erik index for #{fqdn_} updated, downloading partitions from #{length relayUris} relay(s).|]

                -- The worker starts at -N1 to keep its footprint small; now that
                -- there is more than one partition to pull, give it the cores.
                scaleUpCapabilities $ length partitionList
                
                when (indexScope /= fqdn_) $
                    appError $ ErikE $ ErikIndexScopeMismatch { expectedScope = fqdn, actualScope = indexScope }

                logDebug logger [i|Erik index for #{fqdn_} has #{index}.|]
                void $ fmap mconcat $ concurrentlyVTLenientN parallelism partitionList $ \partitionRef@ErikPartitionRef {..} -> do
                    partition <- getPartition partitionRef                                            
                    getManifests indexScope hash partition

                logDebug logger [i|Finished fetching Erik relay #{indexDir} for #{fqdn_}.|]

                -- Per-relay and global query counts, so the spread across
                -- relays and the effect of fall-back are visible.
                stats <- relayStats pool
                logInfo logger [i|Erik relay usage for #{fqdn_}: |] 
                forM_ stats $ \RelayStat {..} ->
                    logInfo logger
                        [i|  #{statRelay}: served=#{statServed} failed=#{statFailed} in-flight=#{statInFlight}|]

                -- Failures are reported by the pool as they happen (see
                -- RelayPool.withRelay); this is the closing summary, which is
                -- what lets a relay that answered clear its failure count.
                pushErikRelayReport logger
                    [ ErikRelayReport statRelay statServed 0
                    | RelayStat {..} <- stats, statServed > 0 ]

                -- Now traverse all downloaded objects and load them into the storage,
                -- the same way it happens for rsync-ed repositories. Do not try to recover 
                -- object locations here.
                loadObjectsFromFS appContext worldVersion (\_ _ -> Nothing) indexDir 
      where
        -- The index is relay state rather than a content-addressed object, so
        -- it is cached per relay: whichever relay the pool ends up serving it
        -- from is the one the cached copy is compared against.
        getIndex :: ValidatorIO es => Eff es (Maybe ErikIndex)
        getIndex = withRelay logger pool $ \relayUri -> do 
            let tmpDir = configValue $ config ^. #tmpDirectory
            let maxSize = config ^. typed @ErikConf . #maxSize
            let theIndexUri = indexUri relayUri
            (indexBs, _, httpStatus, _ignoreEtag) <- 
                    fromTryM (ErikE . Can'tDownloadObject . U.fmtEx) $                                      
                        downloadToBS tmpDir theIndexUri Nothing maxSize
            when (httpStatus /= mempty) $ do 
                appError $ ErikE $ Can'tDownloadObject [i|Could not download index #{theIndexUri}, http status = #{httpStatus}|]

            logDebug logger [i|Downloaded Erik index for #{fqdn_}, HTTP status: #{httpStatus}|]
            index <- parseErikIndex indexBs            

            join $ DB.rwTxT database $ \tx db -> do 
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

        getPartition :: ValidatorIO es => ErikPartitionRef -> Eff es ErikPartition
        getPartition ErikPartitionRef {..} = do 
            z <- DB.roTxT database $ \tx db -> DB.getErikPartition tx db hash
            case z of 
                Nothing -> do     
                    logDebug logger [i|No Erik partition #{U.hashAsBase64Url hash} in the database, downloading from a relay.|]
                    partition <- fetchAndParsePartition
                    DB.rwTxT database $ \tx db -> DB.saveErikPartition tx db hash partition
                    logDebug logger [i|Stored Erik partition #{U.hashAsBase64Url hash} in the database.|]
                    pure partition

                Just partition -> do 
                    logDebug logger [i|Found Erik partition #{U.hashAsBase64Url hash} in the database.|]
                    pure partition
          where
            fetchAndParsePartition :: ValidatorIO es => Eff es ErikPartition
            fetchAndParsePartition = do                                 
                -- It will be cleaned up by the top level
                liftIO $ createDirectoryIfMissing True (partitionDir hash)

                let partitionFile = partitionDir hash </> "partition-" <> show hash

                withRelay logger pool $ \relayUri -> do
                  let partUri = objectByHashUri relayUri hash
                  logDebug logger [i|Downloading Erik partition #{U.hashAsBase64Url hash} from #{partUri} to #{partitionFile}.|]
                  vFocusOn LocationFocus partUri $ do                        
                        (partBs, _, _) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed partUri partitionFile hash size
                                    (\actualStatus -> Left $ ErikE $ Can'tDownloadObject 
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { 
                                        expectedHash = hash, .. })

                        parseErikPartition partBs                      

        getManifests :: (ValidatorIO es, Concurrent :> es) => Text -> Hash -> ErikPartition -> Eff es ()
        getManifests scope partitionHash ErikPartition {..} = do            
            fmap mconcat $ concurrentlyVTLenientN parallelism manifestList $ \manifestRef@ErikManifestRef {..} -> do
                -- RFC §4: all locations in a ManifestRef MUST be encompassed within indexScope
                let badLocations = filter (not . locationInScope scope) locations
                when (not $ null badLocations) $
                    appError $ ErikE $ ErikManifestOutsideScope { location = badLocations, scope = scope }
                
                z <- DB.roTxT database $ \tx db -> DB.getByHash tx db hash
                case z of 
                    Just (Located _ (WellStructuredRO (MftRO mft))) -> do
                        logDebug logger [i|Manifest #{U.hashAsBase64Url hash} already in the database.|]
                        void $ fetchManifestChildren hash (mft ^. #content . #mftEntries)

                    Just (Located _ _) -> do
                        logDebug logger $ [i|Manifest hash #{U.hashAsBase64Url hash} points to an existing |] <>
                                        "object that is not a manifest, it almost surely means broken Erik relay."

                    Nothing -> do
                        mft <- fetchAndParseManifest manifestRef
                        void $ fetchManifestChildren (getHash mft) (getMftChildren mft)

          where
            fetchAndParseManifest ErikManifestRef {..} = do
                
                liftIO $ createDirectoryIfMissing True (manifestDir hash)

                withRelay logger pool $ \relayUri -> do
                  let manifestUri = objectByHashUri relayUri hash
                  vFocusOn LocationFocus manifestUri $ do
                        let manifestFile = manifestDir hash </> show hash <> ".mft"
                        (manifestBs, _, _) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                downloadToFileHashed manifestUri manifestFile hash size
                                    (\actualStatus -> Left $ ErikE $ Can'tDownloadObject 
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })
                        
                        parseMft manifestBs


            fetchManifestChildren :: (ValidatorIO es, Concurrent :> es) => Hash -> [MftPair] -> Eff es (Size, HttpStatus)
            fetchManifestChildren mftHash entries = do       
                let childrenDir_ = childrenDir mftHash
                liftIO $ createDirectoryIfMissing True childrenDir_
                
                let mftChildren = filter (\MftPair {..} -> supportedExtensionByErik $ Text.unpack fileName) entries

                -- This is to avoid a directory with a lot of files in it                
                forM_ (Set.fromList $ map (\MftPair {..} -> U.firstByte hash) mftChildren) $ \firstByte -> do 
                    liftIO $ createDirectoryIfMissing True $ childrenDir_ </> show firstByte

                fmap mconcat $ concurrentlyVTLenientN parallelism mftChildren $ \MftPair {..} -> do 
                    exists <- DB.roTxT database $ \tx db -> DB.hashExists tx db hash
                    if exists then 
                        pure mempty 
                    else do                                             
                        let childFile = childrenDir_ </> show (U.firstByte hash) </> show hash <> "-" <> Text.unpack fileName
                        let maxSize = Size $ fromIntegral $ config ^. #validationConfig . #maxObjectSize                        

                        withRelay logger pool $ \relayUri -> do
                          let childUri = objectByHashUri relayUri hash
                          vFocusOn LocationFocus childUri $ do                            
                                let fetch = 
                                        fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $ 
                                            downloadToFileHashed_ childUri childFile hash maxSize
                                                (\actualStatus -> Left $ ErikE $ Can'tDownloadObject 
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                                (\actualHash -> Left $ ErikE $ ErikHashMismatchError { 
                                                    expectedHash = hash, .. })

                                fetch `catchError` (\cs (e :: AppError) -> do 
                                    logError logger [i|Could not download/parse manifest child #{U.hashAsBase64Url hash} from #{childUri}, error: #{e}.|]
                                    void $ liftIO $ tryAny $ removeFile childFile
                                    rethrowError cs e)
              
            manifestDir mftHash = partitionDir partitionHash </> "m_" <> show (U.firstByte mftHash)
            childrenDir mftHash = manifestDir mftHash </> "ch"


    indexUri relayUri = URI [i|#{relayUri}/.well-known/erik/index/#{fqdn_}|]

    objectByHashUri relayUri hash = let 
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
        