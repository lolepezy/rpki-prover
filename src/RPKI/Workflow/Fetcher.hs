{-# LANGUAGE StrictData #-}

{- | The fetching side of the workflow: one looping thread per repository URL,
     started and stopped as validation discovers and forgets repositories.

     A fetcher tries the primary repository (through the Erik relays when they
     are configured and the repository has been fetched successfully before),
     falls back to the repository's fallbacks when that fails, and records what
     it found. 

     Then it asks for the TAs that depend on the repository to be revalidated, 
     if what it stored changes what the last validation found (see 
     RPKI.Workflow.Revalidation).
-}
module RPKI.Workflow.Fetcher (
    adjustFetchers,
    newFetcher
) where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Lens
import           Data.Generics.Product.Typed

import           Data.Foldable                   (for_)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import qualified Data.Map.Monoidal.Strict        as MonoidalMap
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Maybe                      (fromMaybe)
import qualified Data.IxSet.Typed                as IxSet

import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch.Fetch
import           RPKI.Logging
import           RPKI.Metrics.Prometheus
import           RPKI.Reporting
import           RPKI.Repository
import qualified RPKI.Store.Database             as DB
import           RPKI.Store.Types                (FetchUpdates (..), ManifestUpdates (..), manifestUpdatesCap, nothingStored)
import           RPKI.Parallel
import           RPKI.Time
import           RPKI.Util
import           RPKI.Worker
import           RPKI.Workflow.Revalidation
import           RPKI.Workflow.Shared
import           RPKI.Workflow.Task

import           UnliftIO                        (pooledForConcurrentlyN)


-- | Adjust running fetchers to the latest discovered repositories
-- Updates fetcheables with new ones, creates fetchers for new URLs,
-- and stops fetchers that are no longer needed.
adjustFetchers :: AppContext -> Map TaName Fetcheables -> WorkflowShared -> IO ()
adjustFetchers appContext@AppContext {..} discoveredPerTa workflowShared@WorkflowShared { fetchers = Fetchers {..} } = do
    -- Per TA, so that what is fetched for a TA doesn't depend on
    -- which other TAs happened to be validated together with it.
    let collapsedPerTa = Map.map (collapseRsyncFetcheables (config ^. #rsyncConf . #collapseThreshold)) discoveredPerTa
    let discoveredFetcheables = Map.map fst collapsedPerTa

    for_ (Map.toList collapsedPerTa) $ \(ta, (_, introduced)) ->
        for_ (Map.toList introduced) $ \(parent, count) ->
            logInfo logger [i|TA #{ta}: fetching #{getURL parent} instead of #{count} repositories under it.|]

    (currentFetchers, toStop, toStart) <- atomically $ do

        -- All the URLs that were discovered by the recent validations of all TAs
        relevantUrls :: Set RpkiURL <- do 
                uriByTa_ <- updateUriPerTa discoveredFetcheables <$> readTVar uriByTa
                writeTVar uriByTa uriByTa_
                pure $ Set.fromList $ IxSet.indexKeys uriByTa_
            
        -- This basically means that we filter all the fetcheables 
        -- (new or current) by being amongst the relevant URLs. 
        modifyTVar' fetcheables $ \currentFetcheables -> 
                Fetcheables $ MonoidalMap.filterWithKey 
                    (\url _ -> url `Set.member` relevantUrls) $ 
                    unFetcheables $ currentFetcheables <> mconcat (Map.elems discoveredFetcheables)

        running <- readTVar runningFetchers
        let runningFetcherUrls = Map.keysSet running

        pure (running, 
              Set.difference runningFetcherUrls relevantUrls,
              Set.difference relevantUrls runningFetcherUrls)        

    -- logDebug logger [i|Adjusting fetchers: toStop = #{toStop}, toStart = #{toStart}, currentFetchers = #{Map.keys currentFetchers}|]

    mask_ $ do
        -- Stop and remove fetchers for URLs that are no longer needed    
        for_ (Set.toList toStop) $ \url ->
            for_ (Map.lookup url currentFetchers) $ \thread -> do
                Conc.throwTo thread AsyncCancelled

        threads <- forM (Set.toList toStart) $ \url ->
            (url, ) <$> forkFinally 
                            (newFetcher appContext workflowShared url)
                            (logException logger [i|Exception in fetcher thread for #{url}|])

        atomically $ modifyTVar' runningFetchers $ \r -> 
            foldr Map.delete (foldr (uncurry Map.insert) r threads) (Set.toList toStop)
                        
-- | Create a new fetcher for the given URL and run it.
newFetcher :: AppContext -> WorkflowShared -> RpkiURL -> IO ()
newFetcher appContext@AppContext {..} WorkflowShared { fetchers = fetchers@Fetchers {..}, ..} url = do
    ignoreSync $ go `finally` dropFetcher fetchers url    
  where
    go = case config ^. #proverRunMode of
        OneOffMode _ -> void fetchOnce
        ServerMode   -> do 
            Now start <- thisInstant        
            pauseIfNeeded start
            fetchLoop
      where    
        fetchLoop = do 
            Now start <- thisInstant
            fetchOnce >>= \case        
                Nothing -> do                
                    logInfo logger [i|Fetcher for #{url} is not needed and will be deleted.|]
                Just interval -> do 
                    logDebug logger [i|Fetcher for #{url} finished, next fetch in #{interval}.|]
                    delayUntilNext start interval
                    fetchLoop 

        -- A fetcher that has just been (re)started must not fetch straight away 
        -- if the repository was fetched recently: wait out the rest of its interval.
        pauseIfNeeded now = do 
            f <- fetchableForUrl 
            for_ f $ \_ -> do 
                r <- DB.roTxT database (\tx -> DB.getRepository tx url)
                for_ r $ \repository -> do          
                    let status = getMeta repository ^. #status
                    for_ (fetchMoment status) $ \lastFetch -> do 
                        worldVersion <- newWorldVersion
                        let interval = nextRefreshInterval config repository worldVersion status Nothing 0
                        let pause = leftToWaitMicros (Earlier lastFetch) (Later now) interval                                        
                        when (pause > 0) $ do  
                            let pauseSeconds = pause `div` 1_000_000
                            logDebug logger $ 
                                [i|Fetcher for #{url} finished at #{lastFetch}, now is #{now}, |] <> 
                                [i|interval is #{interval}, first fetch will be paused by #{pauseSeconds}s.|]
                            threadDelay $ fromIntegral pause

    fetchOnce = do 
        fetchableForUrl >>= \case        
            Nothing -> 
                pure Nothing

            Just _ -> do 
                worldVersion <- newWorldVersion
                repository   <- repositoryFor url

                -- TODO It should be refactored to be more systematic: 
                -- If a repository was successfully fetched before, try to fetch the update 
                -- using Erik relays (if configured)
                interval <- case (config ^. typed @ErikConf . #relays, getFetchStatus repository) of 
                    (erikRelays, FetchedAt {}) 
                        | not (null erikRelays) -> 
                            usableErikRelays appState erikRelays >>= \case 
                                [] -> do 
                                    logWarn logger [i|No usable Erik relays for #{url}, falling back to primary fetch.|]
                                    fetchPrimary repository worldVersion
                                usableRelays ->
                                    fetchErikRelays worldVersion repository usableRelays
                    _ -> 
                            fetchPrimary repository worldVersion

                revalidateFor worldVersion
                pure interval
      where
        fetchPrimary repository worldVersion = do
            (r, validations, duration) <- fetchOrJoin url repository worldVersion

            rememberFirstFetchBy worldVersion
            updatePrometheusForRepository url duration prometheusMetrics

            -- TODO Use durationMs, it is the only time metric for failed and killed fetches 
            case r of
                Right (repository', stats) -> do
                    interval <- recordFetchOutcome repository' worldVersion 
                                    (FetchedAt (versionToInstant worldVersion)) stats duration validations
                    pure $ Just interval

                Left _ -> do
                    interval <- recordFetchOutcome repository worldVersion 
                                    (FailedAt (versionToInstant worldVersion)) Nothing duration validations

                    fetchableForUrl >>= \case
                        Nothing -> 
                            -- this whole fetcheable is gone
                            pure Nothing
                        Just fallbacks -> do  
                            -- TODO Maybe try Erik relay before trying rsync
                            anySucceeded <- fetchFallbacks worldVersion fallbacks                                                            
                            pure $ Just $ 
                                if not anySucceeded 
                                    -- nothing responded, so just go with the normal exponential backoff thing
                                    then interval
                                    else case [ () | RsyncU _ <- Set.toList fallbacks ] of                                                     
                                        -- Not implemented yet, in reality it should never happen, 
                                        -- fallbacks can only be rsync in the foreseeable future
                                        [] -> interval
                                        -- fallbacks managed to get through and it was rsync (duh), 
                                        -- so it should be a normal rsync interval then                                                    
                                        _  -> max interval (config ^. #rsyncConf . #repositoryRefreshInterval)

        fetchErikRelays worldVersion repository erikRelays =             
            -- The FQDN is derived out here rather than inside the fetch itself, 
            -- because it is also the key the Erik bookkeeping record is stored under.
            erikFqdnForUrl >>= \case 
                Nothing -> do 
                    logWarn logger [i|Couldn't derive an FQDN for #{url}, fetching it directly.|]
                    fetchPrimary repository worldVersion

                Just fqdn -> do 
                    -- If an Erik fetch for this FQDN happened less than N seconds, skip it.
                    -- Many different repositories can map to the same FQDN be so they will 
                    -- hit the relays for the same FQDN (much) more often than others. Skip 
                    -- this extra repeated fetches.
                    recentFetch fqdn >>= \case
                        Just recentTime -> do                            
                            logDebug logger [i|Skipping Erik fetch for #{fqdn} mapped from #{url} because it was just fetched at #{recentTime}.|]
                            -- Do nothing, keep the same interval
                            pure $ getMeta repository ^. #refreshInterval
                        Nothing -> 
                            doErikFetch fqdn

          where
            recentFetch fqdn = do 
                Now now <- thisInstant
                atomically $ do                         
                    erikFetches <- readTVar lastFqdnFetch                                                                
                    writeTVar lastFqdnFetch $ Map.insert fqdn now erikFetches
                    pure $ case Map.lookup fqdn erikFetches of 
                        Just lastFetch 
                            | closeEnoughMoments (Earlier lastFetch) (Later now) 
                                                    config.erikConf.erikRefreshInterval
                                -> Just lastFetch                                    
                        _ -> Nothing    

            doErikFetch fqdn = do
                (r, validations, duration) <-                 
                    -- A hard cap, not `withFetchLimits`: that one lets a fetch
                    -- through once it has waited long enough, which is right for
                    -- repository fetches but means nothing bounds the number of
                    -- Erik workers, one per FQDN, started in a single round.
                    withSemaphore erikFetchSemaphore $ 
                        runFetch url $ 
                            fetchRepositoryFromErikRelays appContext
                                erikRelays worldVersion fqdn
                case r of 
                    Right ErikFetchStat {..} -> do 
                        let newStatus = FetchedAt (versionToInstant worldVersion)
                        interval <- recordFetchOutcome repository worldVersion newStatus Nothing duration validations
                        saveErikFetchOutcome fqdn newStatus interval relayUsage validations
                        pure $ Just interval

                    Left e -> do
                        logWarn logger [i|Erik relay fetch failed for #{url} with error #{e}, falling back to primary fetch.|]
                        let newStatus = FailedAt (versionToInstant worldVersion)
                        -- Record the failure under the FQDN as well, otherwise the 
                        -- only trace of it is in the log. The repository itself is 
                        -- left alone, the primary fetch below will update it.
                        saveErikFetchOutcome fqdn newStatus 
                            (nextRefreshInterval config repository worldVersion newStatus Nothing duration) 
                            [] validations
                        fetchPrimary repository worldVersion       
         

        fetchFallbacks worldVersion fallbacks = do 
            -- TODO Make it a bit smarter based on the overall number and overall load
            let maxThreads = 32
            repositories <- pooledForConcurrentlyN maxThreads (Set.toList fallbacks) $ \fallbackUrl -> do 

                repository <- repositoryFor fallbackUrl
                (r, validations, duration) <- fetchOrJoin fallbackUrl repository worldVersion

                updatePrometheusForRepository fallbackUrl duration prometheusMetrics
                pure $ case r of
                    Right (repository', _noRrdpStats) -> 
                        -- realistically at this time the only fallback repositories are rsync, so 
                        -- there's no RrdpFetchStat ever
                        (updateMeta' repository' (#status .~ FetchedAt (versionToInstant worldVersion)), 
                         validations, True)
                    Left _ ->
                        (updateMeta' repository (#status .~ FailedAt (versionToInstant worldVersion)), 
                         validations, False)

            DB.rwTxT database $ \tx -> do
                DB.saveRepositories tx [ repo | (repo, _, _) <- repositories ]
                DB.saveRepositoryValidationStates tx [ (repo, vs) | (repo, vs, _) <- repositories ]

            -- Whether any of them got through
            pure $ or [ succeeded | (_, _, succeeded) <- repositories ]
        

    -- Fetch a repository, unless a fetch of the same URL is running already 
    -- (as a fallback of another repository, or as the primary while this is 
    -- a fallback): then wait for it and take its outcome. Two rsync fetches of 
    -- a URL would write to the same directory at once, and after collapsing 
    -- (see 'collapseRsyncFetcheables') one directory can be the fallback of 
    -- many repositories that fail at about the same time. Waiting is outside 
    -- of the fetch limits, so it doesn't take anybody's slot.
    --
    -- That fetch stored objects with its own world version and asked for 
    -- the revalidation of its own TAs, so ask for this fetcher's TAs here.
    fetchOrJoin fetchUrl repository worldVersion = do
        ((fetchedWith, r, validations, duration), joined) <- 
            singleFlight fetchesInProgress fetchUrl $ do 
                (r, validations, duration) <- 
                    withFetchLimits fetchers config repository 
                        $ runConcurrentlyIfPossible logger FetchTask runningTasks 
                            $ runFetch fetchUrl
                                $ fetchRepository appContext worldVersion repository
                pure (worldVersion, r, validations, duration)
        when joined $ do 
            logDebug logger [i|#{fetchUrl} was being fetched already, took the outcome of that fetch.|]
            revalidateFor fetchedWith
        pure (r, validations, duration)

    -- Every fetch attempt is timed and reported under its own repository scope.
    -- It always goes innermost, inside every semaphore and task slot the attempt
    -- has to acquire, so that the duration is the fetch itself and never
    -- includes the time spent queueing for a slot to run in.
    runFetch scopeUrl fetch = do 
        ((r, validations), duration) <- 
            timedMS $ runValidatorIO (newScopes' RepositoryFocus scopeUrl) fetch
        pure (r, validations, duration)

    -- Apply the outcome of a fetch attempt to the repository: the new status and 
    -- the interval until the next attempt, both stored together with the 
    -- validations the attempt produced. Gives the interval back to the fetch loop.
    recordFetchOutcome repository worldVersion newStatus stats duration validations = do 
        let interval = nextRefreshInterval config repository worldVersion newStatus stats duration
        let updated  = updateMeta' repository 
                        (\meta -> meta & #status .~ newStatus 
                                      & #refreshInterval ?~ interval)
        DB.rwTxT database $ \tx -> do
            DB.saveRepositories tx [updated]
            DB.saveRepositoryValidationStates tx [(updated, validations)]
        pure interval

    repositoryFor u = fromMaybe (newRepository u) <$> 
        DB.roTxT database (\tx -> DB.getRepository tx u)

    fetchableForUrl = do 
        Fetcheables fs <- readTVarIO fetcheables
        pure $ MonoidalMap.lookup url fs

    -- The FQDN an Erik fetch works on.
    -- TODO Dirty to extract FQDN from fallback rsync URLs instead of 
    -- RRDP URL, because FQDN comes from SIA of the certificate and 
    -- not from the RRDP host name
    erikFqdnForUrl = do 
        fallbacks <- fromMaybe mempty <$> fetchableForUrl
        let fqdns = Set.fromList [ fqdn | f <- Set.toList fallbacks, Just fqdn <- [getFQDN f]]
        pure $ if Set.null fqdns 
                then getFQDN $ getRpkiURL url
                else Just $ Set.findMin fqdns


    -- Erik fetches are keyed by FQDN and not by repository URL, so they are
    -- tracked separately from the repository itself. Several repositories can
    -- share an FQDN, in which case it is simply the latest fetch that is recorded.
    saveErikFetchOutcome fqdn newStatus interval relayUsage validations =
        DB.rwTxT database $ \tx -> do
            existing <- fromMaybe (newErikRepository fqdn) <$> DB.getErikRepository tx fqdn
            let erikRepository = existing
                    & #meta . #status .~ newStatus
                    & #meta . #refreshInterval ?~ interval
                    -- A failed fetch has nothing to say about the relays, so in
                    -- that case keep whatever the previous one found out.
                    & #relayUsage %~ (\previous -> if null relayUsage then previous else relayUsage)
            DB.saveErikRepositories tx [erikRepository]
            DB.saveErikRepositoryValidationStates tx [(erikRepository, validations)]


    -- Ask for the TAs that use the repository to be revalidated, if what the 
    -- fetch stored changes what the last validation found (see 
    -- RPKI.Workflow.Revalidation). New manifests are compared with the manifest 
    -- shortcuts after the fetch, i.e. as late as possible before the validation 
    -- that uses them.
    --
    -- What the fetch stored is what the cache has with its world version, the 
    -- same for the primary repository, its fallbacks and Erik relays. Also 
    -- after a fetch that failed: it may have stored something before that.
    --
    -- In the one-off mode there is no next round, so every finished fetch has 
    -- to be able to unblock the validation, whether it found anything or not.
    revalidateFor worldVersion = do
        let runMode   = config ^. #proverRunMode
            algorithm = config ^. #validationConfig . #validationAlgorithm
        now <- thisInstant
        (fetchUpdates, verdicts) <- DB.roTxT database $ \tx -> do
            fetchUpdates <- DB.storedByFetch tx worldVersion
            verdicts <- case (runMode, fetchUpdates.manifests) of
                (ServerMode, StoredManifests manifests) -> mapM (manifestVerdict tx algorithm now) manifests
                _                                       -> pure []
            pure (fetchUpdates, verdicts)

        let decision = fetchRevalidation runMode fetchUpdates verdicts
        tas <- Set.fromList . IxSet.indexKeys . IxSet.getEQ url <$> readTVarIO uriByTa
        unless (fetchUpdates == nothingStored && decision == NoRevalidation) $ do
            let stored = case fetchUpdates.manifests of
                    StoredManifests ms -> [i|#{length ms} new manifest(s)|]
                    ManyManifests      -> [i|more than #{manifestUpdatesCap} new manifests|] :: String
                others = if fetchUpdates.newObjects then " and other new objects" else "" :: String
                count v = length $ filter (== v) verdicts
            logDebug logger $ [i|#{url} stored #{stored}#{others}, #{count Revalidate} of them to revalidate for, |] <>
                              [i|#{count WaitingForObjects} waiting for their objects, |] <>
                              [i|#{count NotSignedByCa} not signed by their CA, |] <>
                              [i|#{count UnknownCa} of CAs with no certificate, |] <>
                              [i|decision: #{decision}, TAs: #{Set.toList tas}.|]
        when (decision == RevalidateNow) $
            requestRevalidation tasToValidate tas
    
    rememberFirstFetchBy version = atomically $ do 
        fff <- readTVar firstFinishedFetchBy
        when (Map.notMember url fff) $ 
            writeTVar firstFinishedFetchBy $ Map.insert url version fff
