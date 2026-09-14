{-# LANGUAGE OverloadedStrings  #-}

module RPKI.Fetch.Erik.ErikRelay where

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
import qualified Data.List                       as List
import           Data.Either                     (partitionEithers)
import qualified Data.Set                        as Set
import           Data.Word                       (Word8)

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
import           RPKI.Fetch.Erik.RelayPool
import qualified RPKI.Util as U
import           RPKI.Fetch.Http
import           RPKI.Fetch.DirectoryTraverse
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.Worker
import           RPKI.Time

data IndexFetch index = SameIndex index | UpdatedIndex index
    deriving (Show, Eq, Ord)

{- | One item of Erik work.

     Everything except 'ExpandPartition' is a download; 'ExpandPartition' is the
     bookkeeping half of a partition we already have cached, which still has to
     be walked to find out which of its objects are missing. It goes on the same
     queue so that walk is parallelised and does not hold up the downloads.
-}
data ErikWork
    = FetchPartition ErikPartitionRef
    | ExpandPartition Hash
    | FetchManifest ErikManifestRef
    | FetchChild Hash Text
    deriving stock (Eq, Show)

workHash :: ErikWork -> Hash
workHash = \case
    FetchPartition ref  -> ref.hash
    ExpandPartition h   -> h
    FetchManifest ref   -> ref.hash
    FetchChild h _      -> h

runErikFetchWorker :: ValidatorIO es => AppContext s
                    -> FetchConfig
                    -> WorldVersion
                    -> [URI]
                    -> FQDN
                    -> Eff es ErikFetchStat
runErikFetchWorker appContext fetchConfig worldVersion relayUris fqdn = do
    scopes <- askScopes
    ErikFetchResult z <- runWorker appContext
                            (ErikFetchParams scopes fetchConfig relayUris fqdn worldVersion)
                            (Just $ fetchConfig ^. #erikTimeout)
    embedValidatorT $ pure z

{-
    Implementation of the Erik relay fetcher.
    https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-erik-protocol/

    The download is a work queue drained by worker threads bound to relays (see
    'RPKI.Fetch.Erik.RelayPool'), not a recursive fan-out. Two things follow from
    that, and both are the point of the arrangement:

      * an object referenced from several manifests is downloaded once, because
        the queue is keyed by hash,
      * what is missing locally is worked out set-at-a-time with bulk queries --
        one per index for the partitions, one per partition or manifest for the
        objects below it -- instead of a read transaction per object.
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

    relays <- newRelays perRelayThreads relayUris
    -- Same metric rsync fills in: both end up loading a directory tree through
    -- `loadObjectsFromFS`, which counts the objects into `processed`. Timing it
    -- here is what gives that metric its `totalTimeMs`.
    timedMetric (Proxy :: Proxy TraverseMetric) $ doFetch relays
    -- Whatever happened inside -- a full download, an unchanged index, or a
    -- failure part-way through -- we know which relays answered.
    stats <- relayStats relays

    {- Failures are reported by the pool as they happen (see
       RelayPool.onFailure); this is the closing summary, which is what lets a
       relay that answered clear its failure count in the root process.

       It has to sit out here rather than beside the download, because the
       commonest outcome by far is an index that has not changed: that path does
       no downloading at all, and reporting only from the downloading path would
       mean a healthy relay was almost never credited with anything.
    -}
    pushErikRelayReport logger
        [ ErikRelayReport statRelay statServed 0
        | RelayStat {..} <- stats, statServed > 0 ]

    pure $ toFetchStat stats
  where

    -- Only the relays that actually took part: we know about every configured
    -- relay, and listing the ones that were never touched says nothing about
    -- where the objects came from.
    toFetchStat stats = ErikFetchStat
        [ ErikRelayUsage statRelay statServed statFailed
        | RelayStat {..} <- stats
        , statServed > 0 || statFailed > 0 ]

    parallelism = fromIntegral $ config ^. typed @ErikConf . #parallelism

    {- Threads per relay, which is the only parallelism knob the queue needs:
       the per-relay cap *is* the thread count, and the total is what used to be
       the global cap. Spreading `parallelism` over the relays rather than
       giving each of them `relayParallelism` threads keeps a fetch with many
       relays configured from running far wider than the config asks for.
    -}
    perRelayThreads = let
        perRelayCap = fromIntegral $ config ^. typed @ErikConf . #relayParallelism
        relayCount  = max 1 $ length relayUris
        in max 1 $ min perRelayCap $ (parallelism + relayCount - 1) `div` relayCount

    maxChildSize = Size $ fromIntegral $ config ^. #validationConfig . #maxObjectSize

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

    doFetch relays =
        withDir indexDir $ \_ ->
            U.ifJustM (getIndex relays) $ \(relayUri, index@ErikIndex {..}) -> do
                logInfo logger [i|Erik index for #{fqdn_} updated, downloading partitions from #{length relayUris} relay(s).|]

                when (indexScope /= fqdn_) $
                    appError $ ErikE $ ErikIndexScopeMismatch { expectedScope = fqdn, actualScope = indexScope }

                -- The worker starts at -N1 to keep its footprint small; now that
                -- there is more than one partition to pull, give it the cores.
                scaleUpCapabilities $ length partitionList

                logDebug logger [i|Erik index for #{fqdn_} has #{index}.|]

                -- Every downloaded object lands in one of 256 buckets by the
                -- first byte of its hash, so no directory grows unmanageable and
                -- the whole tree is created once rather than per download.
                liftIO $ do
                    createDirectoryIfMissing True partitionDir
                    forM_ [minBound .. maxBound :: Word8] $ \b ->
                        createDirectoryIfMissing True (objectDir b)

                pool <- newWorkPool

                -- The one query that starts everything: which of the partitions
                -- this index names do we already have?
                cached <- DB.roTxT database $ \tx ->
                            DB.existingErikPartitions tx [ ref.hash | ref <- partitionList ]
                enqueue pool
                    [ (workHash w, w)
                    | ref <- partitionList
                    , let w = if ref.hash `Set.member` cached
                                then ExpandPartition ref.hash
                                else FetchPartition ref ]

                runRelayWorkers logger relays pool (processWork indexScope)

                failures <- poolFailures pool
                done     <- poolSucceeded pool
                logDebug logger [i|Finished fetching Erik relay #{indexDir} for #{fqdn_}: #{done} item(s), #{length failures} failure(s).|]

                -- Per-relay query counts, so the spread across relays and the
                -- effect of fall-back are visible.
                stats <- relayStats relays
                logInfo logger [i|Erik relay usage for #{fqdn_}: |]
                forM_ stats $ \RelayStat {..} ->
                    logInfo logger [i|  #{statRelay}: served=#{statServed} failed=#{statFailed}|]

                -- Now traverse all downloaded objects and load them into the storage,
                -- the same way it happens for rsync-ed repositories. Do not try to recover
                -- object locations here.
                loadObjectsFromFS appContext worldVersion (\_ _ -> Nothing) indexDir

                {- Only a fetch that got everything may record the index as the
                   current one.

                   This is what the next round's "has the index changed?" check
                   reads, and it is the only thing standing between a fetch that
                   died half way and objects that never come back: record the
                   index now and the missing objects stay missing until the
                   publisher happens to change it again. Leaving it unsaved costs
                   one more walk next round, and that walk is cheap because it is
                   a handful of bulk queries.
                -}
                if null failures
                    then DB.rwTxT database $ \tx -> DB.saveErikIndex tx relayUri fqdn index
                    else logWarn logger $
                            [i|Erik fetch for #{fqdn_} could not get #{length failures} object(s), |] <>
                            "not recording the index as up to date so the next round retries them."

      where
        {- The index is relay state rather than a content-addressed object, so
           it is compared per relay: whichever relay the pool ends up serving it
           from is the one the cached copy is compared against.

           Returns 'Nothing' when the index is unchanged, which is the cheap
           path -- every object it names was already downloaded and loaded by
           the round that last recorded it (see the note on 'saveErikIndex').
        -}
        getIndex :: ValidatorIO es => Relays -> Eff es (Maybe (URI, ErikIndex))
        getIndex theRelays = withAnyRelay logger theRelays $ \relayUri -> do
            let tmpDir = configValue $ config ^. #tmpDirectory
            let maxSize = config ^. typed @ErikConf . #maxSize
            let theIndexUri = indexUri relayUri
            (indexBs, _, httpStatus, _ignoreEtag) <-
                    fromTryM (ErikE . Can'tDownloadObject . U.fmtEx) $
                        downloadToBS tmpDir theIndexUri Nothing maxSize
            when (httpStatus /= mempty) $
                appError $ ErikE $ Can'tDownloadObject [i|Could not download index #{theIndexUri}, http status = #{httpStatus}|]

            logDebug logger [i|Downloaded Erik index for #{fqdn_}, HTTP status: #{httpStatus}|]
            index <- parseErikIndex indexBs

            existing <- DB.roTxT database $ \tx -> DB.getErikIndex tx relayUri fqdn
            case existing of
                Just old | old == index -> do
                    logInfo logger [i|Erik index for #{fqdn_} didn't change since the last synchronisation.|]
                    pure Nothing
                Just _ -> do
                    logInfo logger [i|Erik index for #{fqdn_} changed, updating from relay #{relayUri}.|]
                    pure $ Just (relayUri, index)
                Nothing -> do
                    logInfo logger [i|No Erik index for #{fqdn_} in the database, downloading from relay #{relayUri}.|]
                    pure $ Just (relayUri, index)

        {- Handle one queued item, returning whatever new work it revealed.

           An 'AppError' thrown from here means "this relay could not serve this
           item": 'runRelayWorkers' re-queues it against another relay. That is
           why nothing in here retries by itself.
        -}
        processWork :: ValidatorIO es => Text -> URI -> ErikWork -> Eff es [(Hash, ErikWork)]
        processWork scope relayUri = \case
            FetchPartition ref   -> fetchPartition ref
            ExpandPartition h    -> expandCachedPartition h
            FetchManifest ref    -> fetchManifest ref
            FetchChild hash name -> [] <$ fetchChild hash name
          where
            -- | Download a partition and cache it, then walk it like any other.
            fetchPartition ErikPartitionRef {..} = do
                let partUri = objectByHashUri relayUri hash
                let partitionFile = partitionDir </> "partition-" <> show hash
                (partition, ms) <- timedMS $ vFocusOn LocationFocus partUri $
                    withCleanupOnFailure partitionFile $ do
                        (partBs, _, _) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $
                                downloadToFileHashed partUri partitionFile hash size
                                    (\actualStatus -> Left $ ErikE $ Can'tDownloadObject
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError {
                                        expectedHash = hash, .. })
                        parseErikPartition partBs

                DB.rwTxT database $ \tx -> DB.saveErikPartition tx hash partition
                logDebug logger [i|Downloaded Erik partition #{U.hashAsBase64Url hash} from #{partUri} to #{partitionFile}, took #{ms} ms.|]
                -- The partition's hash is already marked as queued, so expanding
                -- it inline rather than queueing an 'ExpandPartition' costs
                -- nothing and saves a round trip through the queue.
                expandPartition hash partition

            {- The partition is cached, so only the walk is left. It is read here
               rather than carried on the queue: an index's partitions together
               name every manifest in the repository, and holding all of those
               lists in memory at once is the one thing this queue must not do.
            -}
            expandCachedPartition partitionHash =
                DB.roTxT database (\tx -> DB.getErikPartition tx partitionHash) >>= \case
                    Just partition -> expandPartition partitionHash partition
                    Nothing        ->
                        -- Deleted between the index query and now (cache cleanup
                        -- collects partitions no index refers to any more). The
                        -- ref is gone from this task, so just re-walk next round.
                        appError $ ErikE $ UnknownErikProblem
                            [i|Erik partition #{U.hashAsBase64Url partitionHash} disappeared from the cache.|]

            {- Walk a partition against the store: queue the manifests we do not
               have, and for the ones we do, queue whichever of their children
               are missing.

               Three queries for the whole partition -- "which of these manifests
               do I have", "give me those manifests", "which of all their children
               do I have" -- where the recursive version used one read transaction
               per manifest and one per child.
            -}
            expandPartition partitionHash ErikPartition {..} = do
                -- RFC 4: all locations in a ManifestRef MUST be encompassed
                -- within indexScope. A ref that breaks the rule is dropped with a
                -- warning rather than failing the partition it came in: the
                -- partition itself verified against its hash, and the rest of
                -- what it names is still worth fetching.
                inScope <- filterM manifestRefInScope manifestList

                have <- DB.roTxT database $ \tx ->
                            DB.existingHashes tx [ ref.hash | ref <- inScope ]

                let (cachedRefs, missingRefs) =
                        List.partition (\ref -> ref.hash `Set.member` have) inScope

                cachedMfts <- DB.roTxT database $ \tx ->
                                DB.getObjectsByHashes tx [ ref.hash | ref <- cachedRefs ]

                let (notManifests, entryLists) = partitionEithers $ map entriesOf cachedMfts
                forM_ notManifests $ \h ->
                    logDebug logger $ [i|Manifest #{U.hashAsBase64Url h} (for Erik download #{fqdn_}) points to an existing |] <>
                                    "object that is not a manifest, it almost surely means broken Erik relay."

                childWork <- childrenToFetch $ concat entryLists
                logDebug logger $
                    [i|Erik partition #{U.hashAsBase64Url partitionHash}: #{length missingRefs} manifest(s) |] <>
                    [i|and #{length childWork} object(s) to download.|]

                pure $ [ (ref.hash, FetchManifest ref) | ref <- missingRefs ] <> childWork
              where
                entriesOf (_, WellStructuredRO (MftRO mft)) = Right (mft ^. #content . #mftEntries)
                entriesOf (h, _)                            = Left h

                manifestRefInScope ErikManifestRef {..} =
                    case filter (not . locationInScope scope) locations of
                        []  -> pure True
                        bad -> do
                            appWarn $ ErikE $ ErikManifestOutsideScope { location = bad, scope = scope }
                            pure False

            fetchManifest ErikManifestRef {..} = do
                let manifestUri = objectByHashUri relayUri hash
                let manifestFile = objectDir (U.firstByte hash) </> show hash <> ".mft"
                (mft, ms) <- timedMS $ vFocusOn LocationFocus manifestUri $
                    withCleanupOnFailure manifestFile $ do
                        (manifestBs, _, _) <-
                            fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $
                                downloadToFileHashed manifestUri manifestFile hash size
                                    (\actualStatus -> Left $ ErikE $ Can'tDownloadObject
                                                        $ U.convert $ "Http status: " <> show actualStatus)
                                    (\actualHash -> Left $ ErikE $ ErikHashMismatchError { expectedHash = hash, .. })
                        parseMft manifestBs

                logDebug logger [i|Downloaded manifest #{U.hashAsBase64Url hash} from #{manifestUri} to #{manifestFile}, took #{ms} ms.|]
                childrenToFetch $ getMftChildren mft

            fetchChild hash fileName = do
                let childUri = objectByHashUri relayUri hash
                let childFile = objectDir (U.firstByte hash) </> show hash <> "-" <> Text.unpack fileName
                vFocusOn LocationFocus childUri $
                    withCleanupOnFailure childFile $
                        fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $
                            downloadToFileHashed_ childUri childFile hash maxChildSize
                                (\actualStatus -> Left $ ErikE $ Can'tDownloadObject
                                        $ U.convert $ "Http status: " <> show actualStatus)
                                (\actualHash -> Left $ ErikE $ ErikHashMismatchError {
                                    expectedHash = hash, .. })

            -- | Which of these manifest entries are not in the store yet, in one
            -- query for the whole manifest (or for a whole partition's worth of
            -- cached manifests).
            childrenToFetch entries = do
                let mftChildren = filter (\e -> supportedExtensionByErik $ Text.unpack e.fileName) entries
                have <- DB.roTxT database $ \tx ->
                            DB.existingHashes tx [ e.hash | e <- mftChildren ]
                pure [ (e.hash, FetchChild e.hash e.fileName)
                     | e <- mftChildren, not (e.hash `Set.member` have) ]

    {- A half-written file has to go.

       The retry is what makes this matter: the next relay writes to the same
       path, and a truncated `<hash>.mft` left behind by a failed attempt is a
       supported extension, so `loadObjectsFromFS` would otherwise pick it up and
       record it as an unparseable object.
    -}
    withCleanupOnFailure file f =
        f `catchError` \cs (e :: AppError) -> do
            void $ liftIO $ tryAny $ removeFile file
            rethrowError cs e

    indexUri relayUri = URI [i|#{relayUri}/.well-known/erik/index/#{fqdn_}|]

    objectByHashUri relayUri hash = let
        niHash = U.hashAsBase64Url hash
        in URI [i|#{relayUri}/.well-known/ni/sha-256/#{niHash}|]

    locationInScope scope (URI uri) = U.getHostname uri == Just scope

    indexDir = let
        tmpDir = configValue $ config ^. #tmpDirectory
        in tmpDir </> "erik" </> U.convert fqdn_

    -- Partitions are not RPKI objects: they are named without an extension so
    -- `loadObjectsFromFS` walks straight past them.
    partitionDir = indexDir </> "p"

    objectDir firstByte = indexDir </> "o" </> show firstByte

    withDir dir f =
        bracketVT
            (createDirectoryIfMissing True dir)
            (\_ -> liftIO $ removeDirectoryRecursive dir)
            (\_ -> f dir)
