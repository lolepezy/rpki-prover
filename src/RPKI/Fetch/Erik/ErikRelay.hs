{-# LANGUAGE OverloadedStrings  #-}

module RPKI.Fetch.Erik.ErikRelay where

import           Effectful.Concurrent (Concurrent)
import           Effectful
import           GHC.Conc                         (getNumCapabilities)
import           Effectful.Error.Static           (catchError, rethrowError)
import           Control.Concurrent.MVar          (MVar, newMVar, withMVar)
import           Control.Concurrent.STM           (readTVarIO)
import           Control.Exception                (evaluate)
import           Control.Lens hiding (index, indices, Indexable)
import           Control.Monad
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Builder         as BB
import qualified Data.ByteString.Lazy            as LBS
import           Data.Generics.Product.Typed
import           Data.Proxy
import           Data.String.Interpolate.IsString
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.List                       as List
import           Data.Either                     (partitionEithers)
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import           Data.Maybe                      (fromMaybe)
import           Data.Hourglass                  (Seconds (..))
import qualified System.Timeout                  as Timeout

import           System.Directory
import           System.FilePath
import           System.IO                       (Handle, IOMode (..), hClose, openBinaryFile)
import           UnliftIO (tryAny)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppMonadUtil
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Cpu                        (useAvailableCpus)
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
import           RPKI.Store.Base.Serialisation   (serialise_, deserialise_)
import           RPKI.Store.Base.Storable        (Compressed (..), toStorableObject)
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

-- | The spill file is a sequence of records, each preceded by its length as
-- 8 bytes little-endian.
encodeLength :: Int -> BS.ByteString
encodeLength = LBS.toStrict . BB.toLazyByteString . BB.word64LE . fromIntegral

-- | Split a spill file back into its records, lazily, so that it is streamed
-- rather than held.
spillRecords :: LBS.ByteString -> [BS.ByteString]
spillRecords bs
    | LBS.null bs = []
    | otherwise   =
        let (lengthBytes, rest) = LBS.splitAt 8 bs
            len                 = LBS.foldr (\b acc -> acc * 256 + fromIntegral b) 0 lengthBytes
            (record, rest')     = LBS.splitAt len rest
        in LBS.toStrict record : spillRecords rest'

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
    AppContext {..}
    worldVersion
    relayUris
    fqdn@(FQDN fqdn_) = do

    relays <- newRelays perRelayThreads relayUris
    -- Same metric rsync fills in: objects stored are counted into `processed`
    -- as they are for an rsync directory load. Timing it here is what gives
    -- that metric its `totalTimeMs`.
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

    {- Every single download from a relay -- index, partition, manifest or
       object -- gets a hard time limit, connection included.

       http-client's own response timeout does not do this: it only bounds the
       wait for response headers, and it is set very high for the sake of RRDP
       snapshots. A relay is a different proposition. It exists to be available
       and quick, and what it serves is small, so a relay that has not delivered
       within a few seconds is not going to be worth waiting for: timing out
       charges it a failure like any other, the item moves to the next relay,
       and a relay that keeps doing it is benched in favour of RRDP and rsync.
    -}
    withinDownloadTimeout :: IO (Either AppError r) -> IO (Either AppError r)
    withinDownloadTimeout download = do
        let Seconds s = config ^. typed @ErikConf . #downloadTimeout
        -- Not 'ErikDownloadTimeout': that one means a whole FQDN fetch ran out
        -- of time, and its message says so. This is one download, and the scope
        -- it is recorded in already names the URL that did not answer.
        fromMaybe (Left $ ErikE $ Can'tDownloadObject [i|no response within #{s} seconds|])
            <$> Timeout.timeout (fromIntegral s * 1_000_000) download

    -- Raise the capability count from the 1 the worker started with, but only
    -- when there is enough independent work to use it, and not above the cores
    -- available (see `useAvailableCpus`).
    scaleUpCapabilities partitionCount = do
        let configured = config ^. typed @Parallelism . #cpuCount
            wanted     = max 1 $ min configured (fromIntegral partitionCount)
        current <- liftIO getNumCapabilities
        when (fromIntegral wanted > current) $ do
            cpus <- liftIO $ useAvailableCpus wanted
            logDebug logger
                [i|Erik worker for #{fqdn_}: #{partitionCount} partition(s), raising -N from #{current} to #{cpus}.|]

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

                liftIO $ createDirectoryIfMissing True partitionDir

                queue <- newWorkQueue

                -- The one query that starts everything: which of the partitions
                -- this index names do we already have?
                cached <- DB.roTxT database $ \tx ->
                            DB.existingErikPartitions tx [ ref.hash | ref <- partitionList ]
                enqueue queue
                    [ (workHash w, w)
                    | ref <- partitionList
                    , let w = if ref.hash `Set.member` cached
                                then ExpandPartition ref.hash
                                else FetchPartition ref ]

                bracketVT
                    (openBinaryFile preparedObjectsFile WriteMode >>= newMVar)
                    (\spill -> liftIO $ withMVar spill hClose)
                    (\spill -> runRelayWorkers logger relays queue (processWork spill indexScope))

                failures <- queueFailures queue
                done     <- queueSucceeded queue
                logDebug logger [i|Finished fetching Erik relay #{indexDir} for #{fqdn_}: #{done} item(s), #{length failures} failure(s).|]

                -- Per-relay query counts, so the spread across relays and the
                -- effect of fall-back are visible.
                stats <- relayStats relays
                logInfo logger [i|Erik relay usage for #{fqdn_}: |]
                forM_ stats $ \RelayStat {..} ->
                    logInfo logger [i|  #{statRelay}: served=#{statServed} failed=#{statFailed}|]

                (_, loadMs) <- timedMS storePreparedObjects
                logInfo logger [i|Stored downloaded Erik objects for #{fqdn_}, took #{loadMs} ms.|]

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
                    vFocusOn LocationFocus theIndexUri $
                        fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $
                            withinDownloadTimeout $
                                Right <$> downloadToBS tmpDir theIndexUri Nothing maxSize
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
        processWork :: ValidatorIO es => MVar Handle -> Text -> URI -> ErikWork -> Eff es [(Hash, ErikWork)]
        processWork spill scope relayUri = \case
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
                                withinDownloadTimeout $ downloadToFileHashed partUri partitionFile hash size
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

            {- The manifest is parsed exactly once, here: the same parse yields
               the children to queue and the row to store. A manifest that does
               not parse at all has no children to offer, so that is a failed
               item, the way it always was; one that parses but fails
               prevalidation is stored like any other broken object.
            -}
            fetchManifest ErikManifestRef {..} = do
                let manifestUri = objectByHashUri relayUri hash
                (mft, ms) <- timedMS $ vFocusOn LocationFocus manifestUri $ do
                    bytes <- downloadObject manifestUri hash size
                    parseAndPrevalidate MFT hash bytes Nothing >>= \case
                        (Right (MftRO mft), lifecycle) -> mft <$ prepareForStorage spill hash lifecycle
                        (Right _, _) -> appError $ ErikE $ UnknownErikProblem
                                            [i|Manifest #{U.hashAsBase64Url hash} parsed as something else.|]
                        (Left e, _)  -> appError e

                logDebug logger [i|Downloaded manifest #{U.hashAsBase64Url hash} from #{manifestUri}, took #{ms} ms.|]
                childrenToFetch $ getMftChildren mft

            fetchChild hash fileName = do
                let childUri = objectByHashUri relayUri hash
                vFocusOn LocationFocus childUri $
                    case nameObjectType $ Text.unpack fileName of
                        Nothing    -> appError $ ErikE $ UnknownErikProblem
                                        [i|Manifest child #{fileName} is not of a type Erik fetches.|]
                        Just type_ -> do
                            bytes          <- downloadObject childUri hash maxChildSize
                            (_, lifecycle) <- parseAndPrevalidate type_ hash bytes Nothing
                            prepareForStorage spill hash lifecycle

            -- | Which of these manifest entries are not in the store yet, in one
            -- query for the whole manifest (or for a whole partition's worth of
            -- cached manifests).
            childrenToFetch entries = do
                let mftChildren = filter (\e -> supportedExtensionByErik $ Text.unpack e.fileName) entries
                have <- DB.roTxT database $ \tx ->
                            DB.existingHashes tx [ e.hash | e <- mftChildren ]
                pure [ (e.hash, FetchChild e.hash e.fileName)
                     | e <- mftChildren, not (e.hash `Set.member` have) ]

    -- | Download an object and check it against its hash. Objects are small
    -- enough to hold, and staying off the disk matters: a file per object costs
    -- several syscalls each, which adds up over a hundred thousand of them.
    downloadObject :: ValidatorIO es' => URI -> Hash -> Size -> Eff es' BS.ByteString
    downloadObject uri hash maxSize =
        fromTryEither (ErikE . Can'tDownloadObject . U.fmtEx) $
            withinDownloadTimeout $ downloadHashedToMemory uri hash maxSize
                (\actualStatus -> ErikE $ Can'tDownloadObject
                                    $ U.convert $ "Http status: " <> show actualStatus)
                (\actualHash -> ErikE $ ErikHashMismatchError { expectedHash = hash, .. })

    {- Reduce a parsed object to the rows it will be stored as, and append them
       to the spill file for 'storePreparedObjects'.

       This is all the work that storing it used to involve apart from the
       INSERTs themselves -- serialising, compressing, extracting what the
       index tables need -- done now, on a download thread, so that none of it
       happens inside the write transaction. It goes to disk rather than
       staying in memory, since the objects of a big FQDN add up to more than a
       worker should hold, and to one file rather than one per object, so it
       is a buffered write and not a round of syscalls.
    -}
    prepareForStorage :: ValidatorIO es' => MVar Handle -> Hash -> RpkiObjectLifecycle -> Eff es' ()
    prepareForStorage spill hash lifecycle = do
        case lifecycle of
            OriginalRO _ vs _ _ -> do
                logError logger [i|Object #{U.hashAsBase64Url hash} failed parse/prevalidation.|]
                embedState vs
            WellStructuredRO _ -> pure ()
        let !record = serialise_ $ DB.prepareObject $ toStorableObject $ Compressed lifecycle
        liftIO $ withMVar spill $ \h -> do
            BS.hPut h $ encodeLength $ BS.length record
            BS.hPut h record

    {- Store everything the download prepared, in one transaction.

       Parsing and serialising have all happened by now, so the transaction
       only streams the spill file back and does the inserts.

       Anything going wrong here fails the whole transaction, and with it the
       fetch: a record that cannot be read back is an object that would
       otherwise be silently missing, and failing means the index is not
       recorded, so the next round tries again.
    -}
    storePreparedObjects :: ValidatorIO es' => Eff es' ()
    storePreparedObjects = do
        db      <- liftIO $ readTVarIO database
        records <- liftIO $ spillRecords <$> LBS.readFile preparedObjectsFile
        DB.rwAppTx db $ \tx ->
            forM_ records $ \record -> do
                prepared <- fromTry (ErikE . UnknownErikProblem . U.fmtEx) $
                                evaluate $ deserialise_ @PreparedObject record
                void $ DB.insertPreparedObject tx prepared worldVersion
                updateMetric @TraverseMetric @_ $
                    #processed %~ Map.unionWith (+) (Map.singleton (Just prepared.objectType) 1)

    preparedObjectsFile = indexDir </> "prepared-objects"

    -- A half-written download has to go: the retry against the next relay
    -- writes to the same path.
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

    partitionDir = indexDir </> "p"

    withDir dir f =
        bracketVT
            (createDirectoryIfMissing True dir)
            (\_ -> liftIO $ removeDirectoryRecursive dir)
            (\_ -> f dir)
