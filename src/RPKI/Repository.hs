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
import           Data.Semigroup

import           Data.X509                   (Certificate)

import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as Set
import           Data.Monoid.Generic

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


data FetchEverSucceeded = Never | AtLeastOnce
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass TheBinary        

instance Monoid FetchEverSucceeded where
    mempty = Never

instance Semigroup FetchEverSucceeded where
    Never       <> Never       = Never
    _           <> _           = AtLeastOnce    

   
data Speed = Quick | Slow | Timedout 
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass TheBinary

data FetchStatus
  = Pending
  | FetchedAt Instant
  | FailedAt Instant  
    deriving stock (Show, Eq, Generic)    
    deriving anyclass TheBinary
    deriving Semigroup via Max FetchStatus

newtype RsyncPublicationPoint = RsyncPublicationPoint { uri :: RsyncURL } 
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass TheBinary

data RrdpRepository = RrdpRepository {
        uri         :: RrdpURL,
        rrdpMeta    :: Maybe (SessionId, RrdpSerial),
        eTag        :: Maybe ETag,
        status      :: FetchStatus,
        speed       :: Speed
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data PublicationPoint = RrdpPP  RrdpRepository | 
                        RsyncPP RsyncPublicationPoint
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary
   
newtype PublicationPointAccess = PublicationPointAccess {
        unPublicationPointAccess :: NonEmpty PublicationPoint
    }
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary        

data Repository = RrdpR RrdpRepository | 
                  RsyncR RsyncRepository
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

data RsyncRepository = RsyncRepository {
        repoPP      :: RsyncPublicationPoint,
        status      :: FetchStatus,
        speed       :: Speed
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data PublicationPoints = PublicationPoints {
        rrdps  :: RrdpMap,
        rsyncs :: RsyncTree,
        lastSucceded :: EverSucceededMap
    } 
    deriving stock (Show, Eq, Ord, Generic)   

newtype RrdpMap = RrdpMap { unRrdpMap :: Map RrdpURL RrdpRepository } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving newtype (Monoid)

newtype EverSucceededMap = EverSucceededMap (Map RpkiURL FetchEverSucceeded)
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
        indivudualFetchRuns    :: TVar (Map RpkiURL (FetchTask (Either AppError Repository, ValidationState))),
        indivudualFetchResults :: TVar (Map RpkiURL ValidationState),
        ppSeqFetchRuns         :: TVar (Map [RpkiURL] (FetchTask [FetchResult])),
        publicationPoints      :: TVar PublicationPoints,
        fetchSemaphore         :: Semaphore
    }
    deriving stock (Eq, Generic)

instance WithRpkiURL PublicationPoint where
    getRpkiURL (RrdpPP RrdpRepository {..})          = RrdpU uri
    getRpkiURL (RsyncPP (RsyncPublicationPoint uri)) = RsyncU uri    

instance WithRpkiURL Repository where
    getRpkiURL (RrdpR RrdpRepository {..}) = RrdpU uri
    getRpkiURL (RsyncR RsyncRepository { repoPP = RsyncPublicationPoint {..} }) = RsyncU uri

instance WithURL RrdpRepository where
    getURL RrdpRepository { uri = RrdpURL u } = u
instance WithURL RsyncRepository where
    getURL RsyncRepository { repoPP = RsyncPublicationPoint {..} } = getURL uri
    

instance Semigroup RrdpRepository where
    r1 <> r2 = 
        if r1 ^. #status >= r2 ^. #status
            then r1 
            else r2 & #uri .~ r1 ^. #uri

-- always use the latest one
instance Ord FetchStatus where
    compare = comparing timeAndStatus
      where             
        timeAndStatus Pending       = (Nothing, 0 :: Int)
        timeAndStatus (FailedAt t)  = (Just t,  0)
        timeAndStatus (FetchedAt t) = (Just t,  1) 

instance Monoid FetchStatus where    
    mempty = Pending
    
instance Monoid Speed where    
    mempty = Quick

instance Semigroup Speed where    
    s1 <> s2 = s2

instance Semigroup EverSucceededMap where
    EverSucceededMap ls1 <> EverSucceededMap ls2 = EverSucceededMap $ Map.unionWith (<>) ls1 ls2        

instance Semigroup RrdpMap where
    RrdpMap rs1 <> RrdpMap rs2 = RrdpMap $ Map.unionWith (<>) rs1 rs2        
    
getFetchStatus :: Repository -> FetchStatus
getFetchStatus (RrdpR r)  = r ^. #status
getFetchStatus (RsyncR r) = r ^. #status

getSpeed :: Repository -> Speed
getSpeed (RrdpR r)  = r ^. #speed
getSpeed (RsyncR r) = r ^. #speed

newPPs :: PublicationPoints
newPPs = PublicationPoints mempty newRsyncTree mempty

newRepositoryProcessing :: Config -> STM RepositoryProcessing
newRepositoryProcessing Config {..} = RepositoryProcessing <$> 
        newTVar mempty <*> 
        newTVar mempty <*>          
        newTVar mempty <*>          
        newTVar newPPs <*>
        createSemaphore (fromIntegral $ parallelism ^. #fetchParallelism)  

addRsyncPrefetchUrls :: Config -> PublicationPoints -> PublicationPoints
addRsyncPrefetchUrls Config {..} pps =     
    foldr (\u pps' -> mergePP (rsyncPP u) pps') pps (rsyncConf ^. #rsyncPrefetchUrls)

newRepositoryProcessingIO :: Config -> IO RepositoryProcessing
newRepositoryProcessingIO = atomically . newRepositoryProcessing

rsyncPP :: RsyncURL -> PublicationPoint
rrdpPP  :: RrdpURL  -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint
rrdpPP = RrdpPP . mkRrdp

rrdpR  :: RrdpURL  -> Repository
rrdpR = RrdpR . mkRrdp

mkRrdp :: RrdpURL -> RrdpRepository
mkRrdp u = RrdpRepository {
        uri      = u,
        rrdpMeta = Nothing,
        eTag     = Nothing,
        status   = Pending,
        speed    = Quick 
    }

rrdpRepository :: PublicationPoints -> RrdpURL -> Maybe RrdpRepository
rrdpRepository (PublicationPoints (RrdpMap rrdps) _ _) u = Map.lookup u rrdps        

rsyncRepository :: PublicationPoints -> RsyncURL -> Maybe RsyncRepository
rsyncRepository (PublicationPoints _ rsyncs _) u = 
    (\(u', info) -> RsyncRepository (RsyncPublicationPoint u') (info ^. typed) (info ^. typed)) 
                    <$> infoInRsyncTree u rsyncs    

repositoryFromPP :: PublicationPoints -> RpkiURL -> Maybe Repository                    
repositoryFromPP pps rpkiUrl = 
    case rpkiUrl of
        RrdpU u  -> RrdpR <$> rrdpRepository pps u
        RsyncU u -> RsyncR <$> rsyncRepository pps u      

mergeRsyncPP :: RsyncPublicationPoint -> PublicationPoints -> PublicationPoints
mergeRsyncPP (RsyncPublicationPoint u) pps = 
    pps & typed %~ toRsyncTree u mempty mempty

mergeRrdp :: RrdpRepository -> PublicationPoints -> PublicationPoints
mergeRrdp r@RrdpRepository { .. } 
        (PublicationPoints (RrdpMap rrdps) rsyncs lastSucceded) =
    PublicationPoints (RrdpMap rrdps') rsyncs lastSucceded'
  where
    rrdps' = case Map.lookup uri rrdps of
                Nothing -> Map.insert uri r rrdps
                Just existing 
                    | r == existing -> rrdps 
                    | otherwise     -> Map.insert uri (r <> existing) rrdps                    
    lastSucceded' = succeededFromStatus (RrdpU uri) status lastSucceded        


succeededFromStatus :: RpkiURL -> FetchStatus -> EverSucceededMap -> EverSucceededMap
succeededFromStatus u (FetchedAt _) lastSucceded = 
    lastSucceded <> EverSucceededMap (Map.singleton u AtLeastOnce)
succeededFromStatus _ _ lastSucceded = lastSucceded


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
    [Change (RpkiURL, FetchEverSucceeded)]


-- | Derive a diff between two states of publication points
changeSet :: PublicationPoints -> PublicationPoints -> ChangeSet
changeSet 
    (PublicationPoints (RrdpMap rrdpOld) (RsyncTree rsyncOld) (EverSucceededMap lastSuccededOld)) 
    (PublicationPoints (RrdpMap rrdpNew) (RsyncTree rsyncNew) (EverSucceededMap lastSuccededNew)) = 
    ChangeSet 
        (putNewRrdps <> removeOldRrdps) 
        (putNewRsyncs <> removeOldRsyncs)
        (putNewSucceded <> removeOldSucceded)
    where
        -- TODO Don't generate removes if there's a PUT for the same URL
        rrdpOldSet = Set.fromList $ Map.elems rrdpOld
        rrdpNewSet = Set.fromList $ Map.elems rrdpNew
        putNewRrdps    = map Put    $ filter (not . (`Set.member` rrdpOldSet)) $ Map.elems rrdpNew        
        removeOldRrdps = map Remove $ filter (not . (`Set.member` rrdpNewSet)) $ Map.elems rrdpOld

        rsyncOldList = Map.toList rsyncOld
        rsyncNewList = Map.toList rsyncNew
        putNewRsyncs    = map Put    $ filter (not . (\(u, p) -> Map.lookup u rsyncOld == Just p)) rsyncNewList        
        removeOldRsyncs = map Remove $ filter (not . (\(u, p) -> Map.lookup u rsyncNew == Just p)) rsyncOldList

        lastSuccededOldList = Map.toList lastSuccededOld
        lastSuccededNewList = Map.toList lastSuccededNew
        putNewSucceded    = map Put    $ filter (not . (\(u, p) -> Map.lookup u lastSuccededOld == Just p)) lastSuccededNewList        
        removeOldSucceded = map Remove $ filter (not . (\(u, p) -> Map.lookup u lastSuccededNew == Just p)) lastSuccededOldList


-- Update statuses of the repositories and last successful fetch times for them
updateStatuses :: Foldable t => PublicationPoints -> t (Repository, FetchStatus, Speed) -> PublicationPoints
updateStatuses 
    (PublicationPoints rrdps rsyncs lastSucceded) newStatuses = 
        PublicationPoints 
            (rrdps <> RrdpMap (Map.fromList rrdpUpdates))
            rsyncsUpdates
            (lastSucceded <> EverSucceededMap (Map.fromList lastSuccededUpdates))
    where
        (rrdpUpdates, rsyncsUpdates, lastSuccededUpdates) = 
            foldr foldRepos ([], rsyncs, []) newStatuses

        foldRepos 
            (RrdpR r@RrdpRepository {..}, newStatus, newSpeed) 
            (rrdps', rsyncs', lastS) = 
                    ((uri, r & #status .~ newStatus & #speed .~ newSpeed) : rrdps', 
                    rsyncs', 
                    status2Success (RrdpU uri) newStatus lastS)

        foldRepos 
            (RsyncR (RsyncRepository (RsyncPublicationPoint uri) _ _), newStatus, newSpeed) 
            (rrdps', rsyncs', lastS) = 
                    (rrdps', 
                    toRsyncTree uri newStatus newSpeed rsyncs', 
                    status2Success (RsyncU uri) newStatus lastS)

        status2Success u (FetchedAt _) lastS = (u, AtLeastOnce) : lastS
        status2Success _ _             lastS = lastS
    

everSucceeded :: PublicationPoints -> RpkiURL -> FetchEverSucceeded
everSucceeded PublicationPoints { lastSucceded = EverSucceededMap m } u = 
    fromMaybe Never $ Map.lookup u m

adjustSucceededUrl :: RpkiURL -> PublicationPoints -> PublicationPoints
adjustSucceededUrl u pps =     
    case findPublicationPointStatus pps of 
        Nothing          -> pps
        Just fetchStatus -> pps & typed %~ succeededFromStatus u fetchStatus
    where        
        findPublicationPointStatus (PublicationPoints (RrdpMap rrdps) rsyncRepos _) =     
            case u of
                RrdpU  rrdpUrl  -> (^. typed) <$> Map.lookup rrdpUrl rrdps
                RsyncU rsyncUrl -> (^. typed) . snd <$> infoInRsyncTree rsyncUrl rsyncRepos  

-- Number of repositories
repositoryCount :: PublicationPoints -> Int
repositoryCount (PublicationPoints (RrdpMap rrdps) (RsyncTree rsyncs) _) =     
    Map.size rrdps + 
    sum (map counts $ Map.elems rsyncs)
  where
    counts (Leaf _) = 1
    counts SubTree {..} = sum $ map counts $ Map.elems rsyncChildren


filterPPAccess :: Config -> PublicationPointAccess -> Maybe PublicationPointAccess    
filterPPAccess Config {..} ppAccess = 
    fmap PublicationPointAccess $ nonEmpty $ 
            NonEmpty.filter filter_ $ 
            unPublicationPointAccess ppAccess
  where
    filter_ pp = 
        case (pp, rrdpConf ^. #enabled, rsyncConf ^. #enabled) of
            (RrdpPP _,  True, _   ) -> True
            (RsyncPP _, _,    True) -> True
            _                       -> False


findSpeedProblems :: PublicationPoints -> [(RpkiURL, Repository)]
findSpeedProblems (PublicationPoints (RrdpMap rrdps) rsyncTree _) = 
    rrdpSpeedProblem <> rsyncSpeedProblem
  where
    rrdpSpeedProblem  = [ (RrdpU u, RrdpR r) 
        | (u, r) <- Map.toList rrdps, r ^. #speed /= Quick ]

    rsyncSpeedProblem = [ (RsyncU u, rsyncRepo u info)
        | (u, info) <- flattenRsyncTree rsyncTree, info ^. #speed /= Quick ]
        where 
            rsyncRepo u info = RsyncR $ RsyncRepository { 
                repoPP = RsyncPublicationPoint u,
                status = info ^. #fetchStatus,
                speed  = info ^. #speed
            }


data RsyncNodeInfo = RsyncNodeInfo {
        fetchStatus :: FetchStatus,
        speed       :: Speed
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving Semigroup via GenericSemigroup RsyncNodeInfo   
    deriving Monoid    via GenericMonoid RsyncNodeInfo

type RsyncNodeNormal = RsyncNode RsyncNodeInfo

newtype RsyncTree = RsyncTree (Map RsyncHost RsyncNodeNormal)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data RsyncNode a = Leaf a
               | SubTree {
                   rsyncChildren :: Map RsyncPathChunk (RsyncNode a)
               } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newRsyncTree :: RsyncTree
newRsyncTree = RsyncTree Map.empty

toRsyncTree :: RsyncURL -> FetchStatus -> Speed -> RsyncTree -> RsyncTree 
toRsyncTree (RsyncURL host path) fetchStatus speed (RsyncTree byHost) = 
    RsyncTree $ Map.alter (Just . maybe 
        (buildRsyncTree path nodeInfo) 
        (pathToRsyncTree path nodeInfo)) host byHost    
  where
    nodeInfo = RsyncNodeInfo fetchStatus speed

pathToRsyncTree :: [RsyncPathChunk] -> RsyncNodeInfo -> RsyncNodeNormal -> RsyncNodeNormal

pathToRsyncTree [] ni (Leaf ni') = Leaf $ ni' <> ni 
pathToRsyncTree [] ni SubTree {} = Leaf ni

-- Strange case when we by some reason decide to merge
-- a deeper nested PP while there's a dowloaded  one some
-- higher level. Not supported, don't change the tree.
pathToRsyncTree _ _ (Leaf ni') = Leaf ni'

pathToRsyncTree (u : us) ni (SubTree ch) = 
    case Map.lookup u ch of
        Nothing    -> SubTree $ Map.insert u (buildRsyncTree us ni) ch            
        Just child -> SubTree $ Map.insert u (pathToRsyncTree us ni child) ch             

buildRsyncTree :: [RsyncPathChunk] -> RsyncNodeInfo -> RsyncNodeNormal
buildRsyncTree [] fs      = Leaf fs
buildRsyncTree (u: us) fs = SubTree $ Map.singleton u $ buildRsyncTree us fs    

infoInRsyncTree :: RsyncURL -> RsyncTree -> Maybe (RsyncURL, RsyncNodeInfo)
infoInRsyncTree (RsyncURL host path) (RsyncTree t) = 
    fetchStatus' path [] =<< Map.lookup host t
  where    
    fetchStatus' _ realPath (Leaf fs) = Just (RsyncURL host realPath, fs)
    fetchStatus' [] _  SubTree {} = Nothing
    fetchStatus' (u: us) realPath SubTree {..} = 
        Map.lookup u rsyncChildren >>= fetchStatus' us (realPath <> [u])

flattenRsyncTree :: RsyncTree -> [(RsyncURL, RsyncNodeInfo)]
flattenRsyncTree (RsyncTree t) = 
    mconcat $ map (\(host, tree) -> flattenTree host tree []) $ Map.toList t    
  where    
    flattenTree host (Leaf info) realPath  = [(RsyncURL host realPath, info)]
    flattenTree host SubTree {..} realPath = 
        mconcat $ map (\(p, n) -> flattenTree host n (realPath <> [p])) $ Map.toList rsyncChildren        
  