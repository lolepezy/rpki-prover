{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.Repository where

import           Control.Lens
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Data.Generics.Product.Typed
import           Data.Ord
import           Data.Monoid.Generic
import           Data.Semigroup

import           Data.X509                   (Certificate)

import           Data.Hourglass
import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Map.Monoidal.Strict    (MonoidalMap(..))
import qualified Data.Map.Monoidal.Strict    as MonoidalMap
import qualified Data.Set                    as Set

import qualified StmContainers.Map           as StmMap

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Config
import           RPKI.RRDP.Types
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Time
import           RPKI.TAL
import           RPKI.Parallel
import           RPKI.Util
import           RPKI.Store.Base.Serialisation

   
data FetchType = Unknown 
               | ForSyncFetch Instant 
               | ForAsyncFetch Instant
    deriving stock (Show, Eq, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via Max FetchType

data FetchStatus = Pending
                 | FetchedAt Instant
                 | FailedAt Instant  
    deriving stock (Show, Eq, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via Max FetchStatus

newtype RsyncPublicationPoint = RsyncPublicationPoint { uri :: RsyncURL } 
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)

data RrdpRepository = RrdpRepository {
        uri       :: RrdpURL,
        rrdpMeta  :: Maybe RrdpMeta,
        eTag      :: Maybe ETag,
        meta      :: RepositoryMeta        
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data PublicationPoint = RrdpPP  RrdpRepository | 
                        RsyncPP RsyncPublicationPoint
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary)
   
{-   
    NOTE: This is an over-generalised version of publication point list, since the current
    RFC prescribes to only fallback from RRDP to rsync publication point, i.e. real instances 
    of this type will always consist of 1 or 2 elements.
    
    However, generic implementation is just simpler (See Fetch.hs module). 
    Whether this generic fetching procedure will be ever useful for fetching from more 
    than one RRDP links -- no idea.
-}   
newtype PublicationPointAccess = PublicationPointAccess {
        unPublicationPointAccess :: NonEmpty PublicationPoint
    }
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary)

data Repository = RrdpR RrdpRepository | 
                  RsyncR RsyncRepository
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary)

data RsyncRepository = RsyncRepository {
        repoPP :: RsyncPublicationPoint,
        meta   :: RepositoryMeta
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data PublicationPoints = PublicationPoints {
        rrdps  :: RrdpMap,
        rsyncs :: RsyncForest
    } 
    deriving stock (Show, Eq, Ord, Generic)       


data RepositoryMeta = RepositoryMeta {
        status            :: FetchStatus,
        fetchType         :: FetchType,
        lastFetchDuration :: Maybe TimeMs,
        refreshInterval   :: Maybe Seconds
    } 
    deriving stock (Show, Eq, Ord, Generic)   
    deriving anyclass (TheBinary)


instance Semigroup RepositoryMeta where
    rm1 <> rm2 = RepositoryMeta { 
        status    = rm1 ^. #status <> rm2 ^. #status,
        fetchType = rm1 ^. #fetchType <> rm2 ^. #fetchType,
        lastFetchDuration = rm2 ^. #lastFetchDuration,
        refreshInterval = rm2 ^. #refreshInterval
    }

instance Monoid RepositoryMeta where
    mempty = RepositoryMeta { 
        status = mempty,
        fetchType = mempty,
        lastFetchDuration = Nothing,
        refreshInterval = Nothing
    }

newtype RrdpMap = RrdpMap { unRrdpMap :: Map RrdpURL RrdpRepository } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving newtype (Monoid)

data FetchResult = 
    FetchSuccess Repository ValidationState | 
    FetchFailure RpkiURL ValidationState    
    deriving stock (Show, Eq, Generic)

data FetchTask a = Stub 
                | Fetching (Async a)                 
    deriving stock (Eq, Ord, Generic)                    

data RepositoryProcessing = RepositoryProcessing {
        fetchRuns              :: StmMap.Map RpkiURL (FetchTask FetchResult),
        indivudualFetchResults :: StmMap.Map RpkiURL ValidationState,
        publicationPoints      :: TVar PublicationPoints,
        fetchSemaphore         :: Semaphore
    }
    deriving stock (Generic)

newtype Fetcheables = Fetcheables { unFetcheables :: MonoidalMap RpkiURL (Set.Set RpkiURL) }    
    deriving stock (Show, Eq, Ord, Generic)  
    deriving anyclass (TheBinary)    
    deriving newtype Monoid
    deriving newtype Semigroup

instance WithRpkiURL PublicationPoint where
    getRpkiURL (RrdpPP RrdpRepository {..})          = RrdpU uri
    getRpkiURL (RsyncPP (RsyncPublicationPoint uri)) = RsyncU uri    

instance WithRpkiURL Repository where
    getRpkiURL (RrdpR RrdpRepository {..}) = RrdpU uri
    getRpkiURL (RsyncR RsyncRepository { repoPP = RsyncPublicationPoint {..} }) = RsyncU uri

instance {-# OVERLAPPING #-} WithURL RrdpRepository where
    getURL RrdpRepository { uri = RrdpURL u } = u

instance {-# OVERLAPPING #-} WithURL RsyncRepository where
    getURL RsyncRepository { repoPP = RsyncPublicationPoint {..} } = getURL uri
    
-- TODO This is shady and I don't remember why is it like this
instance Semigroup RrdpRepository where
    r1 <> r2 = 
        if r1 ^. #meta . #status >= r2 ^. #meta . #status
            then r1 
            else r2 & #uri .~ r1 ^. #uri

-- always use the latest one
instance Ord FetchStatus where
    compare = comparing timeAndStatus
      where             
        timeAndStatus = \case 
            Pending     -> (Nothing, 0 :: Int)
            FailedAt t  -> (Just t,  0)
            FetchedAt t -> (Just t,  1) 

instance Monoid FetchStatus where    
    mempty = Pending
    
instance Monoid FetchType where    
    mempty = Unknown

instance Ord FetchType where
    compare = comparing time
      where             
        time Unknown   = Nothing
        time (ForSyncFetch t) = Just t
        time (ForAsyncFetch t)  = Just t  

instance Semigroup RrdpMap where
    RrdpMap rs1 <> RrdpMap rs2 = RrdpMap $ Map.unionWith (<>) rs1 rs2             

getFetchStatus :: Repository -> FetchStatus
getFetchStatus r = getMeta r ^. #status

getFetchType :: Repository -> FetchType
getFetchType r = getMeta r ^. #fetchType

getMeta :: Repository -> RepositoryMeta
getMeta (RrdpR r)   = r ^. #meta
getMeta (RsyncR r)  = r ^. #meta

isForAsync :: FetchType -> Bool
isForAsync = \case    
    ForAsyncFetch _ -> True
    _               -> False

newPPs :: PublicationPoints
newPPs = PublicationPoints mempty newRsyncForest

newRepository :: RpkiURL -> Repository
newRepository = \case   
    RrdpU u -> RrdpR $ newRrdpRepository u
    RsyncU u -> RsyncR $ newRsyncRepository u

newRrdpRepository :: RrdpURL -> RrdpRepository
newRrdpRepository uri = RrdpRepository {    
        rrdpMeta = Nothing,
        eTag     = Nothing,
        meta     = mempty,
        uri      = uri
    }

newRsyncRepository :: RsyncURL -> RsyncRepository
newRsyncRepository url = RsyncRepository {
        repoPP = RsyncPublicationPoint url,
        meta   = mempty
    }

updateMeta' :: Repository -> (RepositoryMeta -> RepositoryMeta) -> Repository
updateMeta' (RrdpR r) newMeta = RrdpR $ r & #meta %~ newMeta
updateMeta' (RsyncR r) newMeta = RsyncR $ r & #meta %~ newMeta

newRepositoryProcessing :: Config -> STM RepositoryProcessing
newRepositoryProcessing Config {..} = RepositoryProcessing <$> 
        StmMap.new <*>               
        StmMap.new <*>
        newTVar newPPs <*>
        newSemaphore (fromIntegral $ parallelism ^. #fetchParallelism)  

newFetcheables :: RpkiURL -> Maybe RpkiURL -> Fetcheables
newFetcheables primary fallback = Fetcheables $ 
    MonoidalMap.singleton primary (maybe Set.empty Set.singleton fallback)

addRsyncPrefetchUrls :: Config -> PublicationPoints -> PublicationPoints
addRsyncPrefetchUrls Config {..} pps =     
    foldr (mergePP . rsyncPP) pps (rsyncConf ^. #rsyncPrefetchUrls)

newRepositoryProcessingIO :: Config -> IO RepositoryProcessing
newRepositoryProcessingIO = atomically . newRepositoryProcessing

rsyncPP :: RsyncURL -> PublicationPoint
rrdpPP  :: RrdpURL  -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint
rrdpPP = RrdpPP . newRrdpRepository

rrdpRepository :: PublicationPoints -> RrdpURL -> Maybe RrdpRepository
rrdpRepository PublicationPoints { rrdps = RrdpMap rrdps } u = Map.lookup u rrdps        

rsyncRepository :: PublicationPoints -> RsyncURL -> Maybe RsyncRepository
rsyncRepository PublicationPoints {..} rsyncUrl = 
    (\(u, meta) -> RsyncRepository (RsyncPublicationPoint u) meta)
                    <$> lookupInRsyncForest rsyncUrl rsyncs    

repositoryFromPP :: PublicationPoints -> PublicationPoint -> Maybe Repository
repositoryFromPP pps pp = 
    case getRpkiURL pp of
        RrdpU u  -> RrdpR <$> rrdpRepository merged u
        RsyncU u -> RsyncR <$> rsyncRepository merged u
  where 
    merged = mergePP pp pps        
    

mergeRsyncPP :: RsyncPublicationPoint -> PublicationPoints -> PublicationPoints
mergeRsyncPP (RsyncPublicationPoint u) pps = 
    pps & typed @RsyncForest %~ toRsyncForest u mempty

mergeRrdp :: RrdpRepository -> PublicationPoints -> PublicationPoints
mergeRrdp r@RrdpRepository {..} pps =
    pps & #rrdps %~ newRrdps
  where
    newRrdps (RrdpMap rrdps) = RrdpMap $ 
        case Map.lookup uri rrdps of
            Nothing -> Map.insert uri r rrdps
            Just existing 
                | r == existing -> rrdps 
                | otherwise     -> Map.insert uri (r <> existing) rrdps                        

mergePP :: PublicationPoint -> PublicationPoints -> PublicationPoints
mergePP (RrdpPP r) = mergeRrdp r
mergePP (RsyncPP r) = mergeRsyncPP r    

    
-- | Extract repositories from URIs in TAL and in TA certificate,
-- | use some reasonable heuristics, but don't try to be very smart.
-- | Prefer RRDP to rsync for everything.
publicationPointsFromTAL :: TAL -> CaCerObject -> Either ValidationError PublicationPointAccess
publicationPointsFromTAL tal (cwsX509certificate . getCertWithSignature -> cert) = 
    case tal of 
        PropertiesTAL {..} -> do 

            PublicationPointAccess ppsFromCert <- getPublicationPointsFromCert cert
            
            let uniquePrefetchRepos = map (snd . fromURI) prefetchUris

            let prefetchReposToUse = 
                    List.sort [ r | r@(RrdpPP _) <- uniquePrefetchRepos ] <>
                    List.sort [ r | r@(RsyncPP _) <- uniquePrefetchRepos ]
            
            pure $ PublicationPointAccess $ 
                maybe ppsFromCert (ppsFromCert <>) $ 
                nonEmpty prefetchReposToUse

        RFC_TAL {} -> getPublicationPointsFromCert cert
  where        
    fromURI r = 
        case r of
            RrdpU u  -> (r, rrdpPP u)
            RsyncU u -> (r, rsyncPP u)           


-- | Get publication points of the certificate.
-- 
getPublicationPointsFromCertObject :: CaCerObject -> Either ValidationError PublicationPointAccess
getPublicationPointsFromCertObject = getPublicationPointsFromCert . cwsX509certificate . getCertWithSignature

getPublicationPointsFromCert :: Certificate -> Either ValidationError PublicationPointAccess
getPublicationPointsFromCert cert = do 
    rrdp <- case getRrdpNotifyUri cert of 
                Just rrdpNotifyUri
                    | isRrdpURI rrdpNotifyUri -> Right [rrdpPP $ RrdpURL rrdpNotifyUri]
                    | otherwise               -> Left $ UnknownUriType rrdpNotifyUri
                Nothing -> Right []

    rsync <- case getRepositoryUri cert of 
                Just repositoryUri
                    | isRsyncURI repositoryUri -> 
                        case parseRsyncURL (unURI repositoryUri) of 
                            Left e   -> Left $ BrokenUri (unURI repositoryUri) e
                            Right rr -> Right [rsyncPP rr]                        
                    | otherwise -> Left $ UnknownUriType repositoryUri
                Nothing -> Right []

    case nonEmpty (rrdp <> rsync) of 
        Nothing -> Left CertificateDoesntHaveSIA
        Just ne -> Right $ PublicationPointAccess ne
    

data Change a = Put a | Remove a 
    deriving stock (Show, Eq, Ord, Generic)

data ChangeSet = ChangeSet
    [Change RrdpRepository]    
    [Change (RsyncHost, RsyncNodeNormal)]    


-- | Derive a diff between two states of publication points
changeSet :: PublicationPoints -> PublicationPoints -> ChangeSet
changeSet 
    (PublicationPoints (RrdpMap rrdpDb) (RsyncForestGen rsyncDb)) 
    (PublicationPoints (RrdpMap rrdpNew) (RsyncForestGen rsyncNew)) = 
    ChangeSet 
        (mergedRrdp <> newRrdp <> rrdpToDelete) 
        (putNewRsyncs <> removeOldRsyncs)                
    where
        rrdps' = map (\(u, new) -> (new, Map.lookup u rrdpDb)) $ Map.toList rrdpNew

        newRrdp  = map Put [ new | (new, Nothing) <- rrdps' ]

        -- We trust RRDP meta from the DB more -- it has been updated by the 
        -- delta/snapshot fetchers in the same transactions as the data
        mergedRrdp = map Put [ new { rrdpMeta = dbRrdpMeta } | 
                                (new, Just (RrdpRepository { rrdpMeta = dbRrdpMeta })) <- rrdps' ]

        rrdpToDelete = map Remove [ r | (u, r) <- Map.toList rrdpDb, not (u `Map.member` rrdpNew) ]

        rsyncOldList = Map.toList rsyncDb
        rsyncNewList = Map.toList rsyncNew
        putNewRsyncs    = map Put    $ filter (not . (\(u, p) -> Map.lookup u rsyncDb == Just p)) rsyncNewList        
        removeOldRsyncs = map Remove $ filter (not . (\(u, p) -> Map.lookup u rsyncNew == Just p)) rsyncOldList                


-- Update statuses of the repositories and last successful fetch times for them
updateMeta :: Foldable t => PublicationPoints -> t (Repository, RepositoryMeta) -> PublicationPoints
updateMeta 
    (PublicationPoints rrdps rsyncs) newMetas = 
        PublicationPoints 
            (rrdps <> RrdpMap (Map.fromList rrdps_))
            rsyncs_ 
    where
        (rrdps_, rsyncs_) = 
            foldr foldRepos ([], rsyncs) newMetas

        foldRepos 
            (RrdpR r@RrdpRepository {..}, newMeta) 
            (rrdps', rsyncs') = 
                ((uri, r & #meta .~ newMeta) : rrdps', rsyncs')

        foldRepos 
            (RsyncR (RsyncRepository (RsyncPublicationPoint uri) _), newMeta) 
            (rrdps', rsyncs') = 
                (rrdps', toRsyncForest uri newMeta rsyncs')            

-- Number of repositories
repositoryCount :: PublicationPoints -> Int
repositoryCount (PublicationPoints (RrdpMap rrdps) (RsyncForestGen rsyncs)) =     
    Map.size rrdps + 
    sum (map rsyncCounts $ Map.elems rsyncs)
  where
    rsyncCounts (Leaf _) = 1
    rsyncCounts SubTree {..} = sum $ map rsyncCounts $ Map.elems rsyncChildren


filterPPAccess :: Config -> PublicationPointAccess -> Maybe PublicationPointAccess    
filterPPAccess Config {..} ppAccess = 
    fmap PublicationPointAccess $ nonEmpty $ 
            NonEmpty.filter filter_ $ 
            unPublicationPointAccess ppAccess
  where
    filter_ = \case
        RrdpPP _  -> rrdpConf ^. #enabled
        RsyncPP _ -> rsyncConf ^. #enabled        


findRepositoriesForAsyncFetch :: PublicationPoints -> [(RpkiURL, Repository)]
findRepositoriesForAsyncFetch (PublicationPoints (RrdpMap rrdps) rsyncTree) = 
    rrdpSpeedProblem <> rsyncSpeedProblem
  where
    rrdpSpeedProblem  = [ (RrdpU u, RrdpR r) 
        | (u, r) <- Map.toList rrdps, isForAsync $ r ^. #meta . #fetchType ]

    rsyncSpeedProblem = [ (RsyncU u, rsyncRepo u meta)
        | (u, meta@RepositoryMeta {..}) 
            <- flattenRsyncTree rsyncTree, isForAsync fetchType ]
      where 
        rsyncRepo (RsyncPublicationPoint -> repoPP) meta = RsyncR $ RsyncRepository {..}


type RsyncNodeNormal = RsyncTree RepositoryMeta
type RsyncSuffixTree = RsyncTree ()

-- Simple tree for representing rsync repositories grouped by host.
-- Every RsyncTree corresponds to a path chunk in the rsync URL. 

type RsyncForest = RsyncForestGen RepositoryMeta
type RsyncForestNoContent = RsyncForestGen ()
    
newtype RsyncForestGen a = RsyncForestGen (Map RsyncHost (RsyncTree a))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data RsyncTree a = Leaf a
               | SubTree {
                   rsyncChildren :: Map RsyncPathChunk (RsyncTree a)
               } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newRsyncForest :: RsyncForest
newRsyncForest = RsyncForestGen Map.empty

toRsyncForest :: Semigroup a => RsyncURL -> a -> RsyncForestGen a -> RsyncForestGen a
toRsyncForest (RsyncURL host path) a (RsyncForestGen byHost) = 
    RsyncForestGen $ Map.alter (Just . maybe 
        (buildRsyncTree path a) 
        (pathToRsyncTree path a)) host byHost      

pathToRsyncTree :: Semigroup a => [RsyncPathChunk] -> a -> RsyncTree a -> RsyncTree a

pathToRsyncTree [] a (Leaf existingA) = Leaf $ existingA <> a
pathToRsyncTree [] a SubTree {} = Leaf a

-- Strange case when we by some reason decide to merge
-- a deeper nested PP while there's a dowloaded  one some
-- higher level. Not supported, don't change the tree.
pathToRsyncTree _ _ (Leaf a) = Leaf a

pathToRsyncTree (u : us) a (SubTree ch) = 
    case Map.lookup u ch of
        Nothing    -> SubTree $ Map.insert u (buildRsyncTree us a) ch            
        Just child -> SubTree $ Map.insert u (pathToRsyncTree us a child) ch             

buildRsyncTree :: [RsyncPathChunk] -> a -> RsyncTree a
buildRsyncTree [] fs      = Leaf fs
buildRsyncTree (u: us) fs = SubTree $ Map.singleton u $ buildRsyncTree us fs    

lookupInRsyncForest :: RsyncURL -> RsyncForestGen a -> Maybe (RsyncURL, a)
lookupInRsyncForest (RsyncURL host path) (RsyncForestGen t) = do
    (path', a) <- lookupInRsyncTree path =<< Map.lookup host t
    pure (RsyncURL host path', a)

lookupInRsyncTree :: [RsyncPathChunk] -> RsyncTree a -> Maybe ([RsyncPathChunk], a)
lookupInRsyncTree path = meta_ path []
  where    
    meta_ _ realPath (Leaf fs) = Just (realPath, fs)
    meta_ [] _  SubTree {} = Nothing
    meta_ (u: us) realPath SubTree {..} = 
        Map.lookup u rsyncChildren >>= meta_ us (realPath <> [u])


flattenRsyncTree :: RsyncForestGen a -> [(RsyncURL, a)]
flattenRsyncTree (RsyncForestGen t) = 
    concatMap (\(host, tree) -> flattenTree host tree []) $ Map.toList t    
  where    
    flattenTree host (Leaf info) realPath  = [(RsyncURL host (reverse realPath), info)]
    flattenTree host SubTree {..} realPath = 
        concatMap (\(p, n) -> flattenTree host n (p : realPath)) $ Map.toList rsyncChildren  
