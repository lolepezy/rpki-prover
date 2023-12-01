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
import           Data.Hashable
import qualified Data.Set                    as Set
import           Data.Monoid.Generic

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


data FetchEverSucceeded = Never | AtLeastOnce
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass TheBinary        

instance Monoid FetchEverSucceeded where
    mempty = Never

instance Semigroup FetchEverSucceeded where
    Never       <> Never       = Never
    _           <> _           = AtLeastOnce    

   
data Speed = Unknown 
            | Quick Instant 
            | Slow Instant
    deriving stock (Show, Eq, Generic)    
    deriving anyclass TheBinary
    deriving Semigroup via Max Speed

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
        -- Set of _slow_ URL that were requested to fetch as a result of fetching 
        -- publication points on certificats during the last validation.
        -- In other words, slow and timing out repository URLs we care about 
        -- and want to keep up-to-date in the local cache.
        slowRequested :: Set.Set [RpkiURL]
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

instance Hashable PublicationPointAccess where  
    hashWithSalt s (PublicationPointAccess ppas) = 
        hashWithSalt s $ map (unURI . getURL . getRpkiURL) $ NonEmpty.toList ppas

data RepositoryProcessing = RepositoryProcessing {
        individualFetchRuns    :: StmMap.Map RpkiURL (FetchTask (Either AppError Repository, ValidationState)),
        indivudualFetchResults :: StmMap.Map RpkiURL ValidationState,
        ppSeqFetchRuns         :: StmMap.Map [RpkiURL] (FetchTask [FetchResult]),
        publicationPoints      :: TVar PublicationPoints,
        fetchSemaphore         :: Semaphore
    }
    deriving stock (Generic)

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
        timeAndStatus = \case 
            Pending     -> (Nothing, 0 :: Int)
            FailedAt t  -> (Just t,  0)
            FetchedAt t -> (Just t,  1) 

instance Monoid FetchStatus where    
    mempty = Pending
    
instance Monoid Speed where    
    mempty = Unknown

instance Ord Speed where
    compare = comparing time
      where             
        time Unknown   = Nothing
        time (Quick t) = Just t
        time (Slow t)  = Just t

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

isSlow :: Speed -> Bool
isSlow = \case    
    Slow _ -> True
    _      -> False

newPPs :: PublicationPoints
newPPs = PublicationPoints mempty newRsyncTree mempty

newRepositoryProcessing :: Config -> STM RepositoryProcessing
newRepositoryProcessing Config {..} = RepositoryProcessing <$> 
        StmMap.new <*> 
        StmMap.new <*>          
        StmMap.new <*>                  
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
        speed    = Unknown 
    }

rrdpRepository :: PublicationPoints -> RrdpURL -> Maybe RrdpRepository
rrdpRepository PublicationPoints { rrdps = RrdpMap rrdps } u = Map.lookup u rrdps        

rsyncRepository :: PublicationPoints -> RsyncURL -> Maybe RsyncRepository
rsyncRepository PublicationPoints {..} u = 
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
mergeRrdp r@RrdpRepository {..} pps =
    pps & #rrdps %~ newRrdps
  where
    newRrdps (RrdpMap rrdps) = RrdpMap $ 
        case Map.lookup uri rrdps of
            Nothing -> Map.insert uri r rrdps
            Just existing 
                | r == existing -> rrdps 
                | otherwise     -> Map.insert uri (r <> existing) rrdps                        


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
    (Change (Set.Set [RpkiURL]))


-- | Derive a diff between two states of publication points
changeSet :: PublicationPoints -> PublicationPoints -> ChangeSet
changeSet 
    (PublicationPoints (RrdpMap rrdpOld) (RsyncTree rsyncOld)  _ ) 
    (PublicationPoints (RrdpMap rrdpNew) (RsyncTree rsyncNew) requestNew) = 
    ChangeSet 
        (putNewRrdps <> removeOldRrdps) 
        (putNewRsyncs <> removeOldRsyncs)        
        (Put requestNew)
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


-- Update statuses of the repositories and last successful fetch times for them
updateStatuses :: Foldable t => PublicationPoints -> t (Repository, FetchStatus, Speed) -> PublicationPoints
updateStatuses 
    (PublicationPoints rrdps rsyncs slowRequested) newStatuses = 
        PublicationPoints 
            (rrdps <> RrdpMap (Map.fromList rrdpUpdates))
            rsyncsUpdates slowRequested
    where
        (rrdpUpdates, rsyncsUpdates) = 
            foldr foldRepos ([], rsyncs) newStatuses

        foldRepos 
            (RrdpR r@RrdpRepository {..}, newStatus, newSpeed) 
            (rrdps', rsyncs') = 
                ((uri, r & #status .~ newStatus & #speed .~ newSpeed) : rrdps', rsyncs')

        foldRepos 
            (RsyncR (RsyncRepository (RsyncPublicationPoint uri) _ _), newStatus, newSpeed) 
            (rrdps', rsyncs') = 
                (rrdps', toRsyncTree uri newStatus newSpeed rsyncs')            

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
    filter_ = \case
        RrdpPP _  -> rrdpConf ^. #enabled
        RsyncPP _ -> rsyncConf ^. #enabled        


findSpeedProblems :: PublicationPoints -> [(RpkiURL, Repository)]
findSpeedProblems (PublicationPoints (RrdpMap rrdps) rsyncTree _) = 
    rrdpSpeedProblem <> rsyncSpeedProblem
  where
    rrdpSpeedProblem  = [ (RrdpU u, RrdpR r) 
        | (u, r) <- Map.toList rrdps, isSlow $ r ^. #speed ]

    rsyncSpeedProblem = [ (RsyncU u, rsyncRepo u info)
        | (u, info) <- flattenRsyncTree rsyncTree, isSlow $ info ^. #speed ]
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
  