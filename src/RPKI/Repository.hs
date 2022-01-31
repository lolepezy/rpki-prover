{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.Repository where

import           Codec.Serialise
import           Control.Lens

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Data.Generics.Product.Typed
import           Data.Ord

import           Data.X509                   (Certificate)

import           Data.List.NonEmpty          (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as Set

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Config
import           RPKI.RRDP.Types
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Time
import           RPKI.TAL
import           RPKI.Util


data FetchEverSucceeded = Never | AtLeastOnce
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass Serialise        

instance Monoid FetchEverSucceeded where
    mempty = Never

instance Semigroup FetchEverSucceeded where
    Never       <> Never       = Never
    _           <> _           = AtLeastOnce    
    

data FetchStatus
  = Pending
  | FetchedAt Instant
  | FailedAt Instant  
    deriving stock (Show, Eq, Generic)    
    deriving anyclass Serialise

newtype RsyncPublicationPoint = RsyncPublicationPoint { uri :: RsyncURL } 
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass Serialise

data RrdpRepository = RrdpRepository {
        uri         :: RrdpURL,
        rrdpMeta    :: Maybe (SessionId, RrdpSerial),
        status      :: FetchStatus
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PublicationPoint = RrdpPP  RrdpRepository | 
                        RsyncPP RsyncPublicationPoint
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

newtype RepositoryAccess = RepositoryAccess {
        unRepositoryAccess :: NonEmpty Repository
    }
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise        

newtype PublicationPointAccess = PublicationPointAccess {
        unPublicationPointAccess :: NonEmpty PublicationPoint
    }
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise        

data Repository = RrdpR RrdpRepository | 
                  RsyncR RsyncRepository
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data RsyncRepository = RsyncRepository {
        repoPP      :: RsyncPublicationPoint,
        status      :: FetchStatus
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PublicationPoints = PublicationPoints {
        rrdps  :: RrdpMap,
        rsyncs :: RsyncTree,
        lastSucceded :: EverSucceededMap
    } 
    deriving stock (Show, Eq, Ord, Generic)   

newtype RrdpMap = RrdpMap { unRrdpMap :: Map RrdpURL RrdpRepository } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid)

newtype EverSucceededMap = EverSucceededMap (Map RpkiURL FetchEverSucceeded)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
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
        publicationPoints      :: TVar PublicationPoints        
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
    RrdpRepository { uri = u1, rrdpMeta = m1, status = s1 } <> 
        RrdpRepository { rrdpMeta = m2, status = s2 } = 
        RrdpRepository u1 resultMeta resultStatus
      where
        (resultStatus, resultMeta) = 
            if s1 >= s2 
                then (s1, m1)
                else (s2, m2)

-- always use the latest one
instance Ord FetchStatus where
    compare = comparing timeAndStatus
      where             
        timeAndStatus Pending       = (Nothing, 0 :: Int)
        timeAndStatus (FailedAt t)  = (Just t,  0)
        timeAndStatus (FetchedAt t) = (Just t,  1) 

instance Semigroup FetchStatus where
    (<>) = max

instance Semigroup EverSucceededMap where
    EverSucceededMap ls1 <> EverSucceededMap ls2 = EverSucceededMap $ Map.unionWith (<>) ls1 ls2        

instance Semigroup RrdpMap where
    RrdpMap rs1 <> RrdpMap rs2 = RrdpMap $ Map.unionWith (<>) rs1 rs2        
    
getFetchStatus :: Repository -> FetchStatus
getFetchStatus (RrdpR r)  = r ^. #status
getFetchStatus (RsyncR r) = r ^. #status

newPPs :: PublicationPoints
newPPs = PublicationPoints mempty newRsyncTree mempty

newRepositoryProcessing :: STM RepositoryProcessing
newRepositoryProcessing = RepositoryProcessing <$> 
        newTVar mempty <*> 
        newTVar mempty <*>          
        newTVar mempty <*>          
        newTVar newPPs 

newRepositoryProcessingIO :: IO RepositoryProcessing
newRepositoryProcessingIO = atomically newRepositoryProcessing

rsyncPP :: RsyncURL -> PublicationPoint
rrdpPP  :: RrdpURL  -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint
rrdpPP u = RrdpPP $ RrdpRepository u Nothing Pending


rrdpRepository :: PublicationPoints -> RrdpURL -> Maybe RrdpRepository
rrdpRepository (PublicationPoints (RrdpMap rrdps) _ _) u = Map.lookup u rrdps        

rsyncRepository :: PublicationPoints -> RsyncURL -> Maybe RsyncRepository
rsyncRepository (PublicationPoints _ rsyncs _) u = 
    (\(u', status) -> RsyncRepository (RsyncPublicationPoint u') status) 
                    <$> statusInRsyncTree u rsyncs    

repositoryFromPP :: PublicationPoints -> RpkiURL -> Maybe Repository                    
repositoryFromPP pps rpkiUrl = 
    case rpkiUrl of
        RrdpU u  -> RrdpR <$> rrdpRepository pps u
        RsyncU u -> RsyncR <$> rsyncRepository pps u      

mergeRsyncPP :: RsyncPublicationPoint -> PublicationPoints -> PublicationPoints
mergeRsyncPP (RsyncPublicationPoint u) pps = 
    pps & typed %~ toRsyncTree u Pending

mergeRrdp :: RrdpRepository -> PublicationPoints -> PublicationPoints
mergeRrdp r@RrdpRepository { .. } 
        (PublicationPoints (RrdpMap rrdps) rsyncs lastSucceded) =
    PublicationPoints (RrdpMap rrdps') rsyncs lastSucceded'
  where
    rrdps' = case Map.lookup uri rrdps of
                Nothing -> Map.insert uri r rrdps
                Just existing 
                    | r == existing -> rrdps 
                    | otherwise     -> Map.insert uri r rrdps                    
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
-- | URI of the repository is supposed to be a "real" one, i.e. where
-- | repository can actually be downloaded from.
publicationPointsFromTAL :: TAL -> CerObject -> Either ValidationError PublicationPointAccess
publicationPointsFromTAL tal (cwsX509certificate . getCertWithSignature -> cert) = 
    case tal of 
        PropertiesTAL {..} -> do 
            (certUri, publicationPoint) <- publicationPointsFromCert cert
            let uniquePrefetchRepos :: [PublicationPoint] = map 
                    (snd . fromURI) $ filter (/= certUri) prefetchUris

            let prefetchReposToUse = 
                    case [ r | r@(RrdpPP _) <- uniquePrefetchRepos ] of
                        []    -> uniquePrefetchRepos
                        rrdps -> rrdps

            pure $ PublicationPointAccess $ publicationPoint :| prefetchReposToUse 

        RFC_TAL {} -> do 
            (_, pp) <- publicationPointsFromCert cert            
            pure $ PublicationPointAccess $ pp :| []
  where        
    fromURI r = 
        case r of
            RrdpU u  -> (r, rrdpPP u)
            RsyncU u -> (r, rsyncPP u)    
        

-- | Create repository from the publication points of the certificate.
publicationPointsFromCert :: Certificate -> Either ValidationError (RpkiURL, PublicationPoint)
publicationPointsFromCert cert = 
    case (getRrdpNotifyUri cert, getRepositoryUri cert) of
        (Just rrdpNotifyUri, _) 
            | isRrdpURI rrdpNotifyUri -> let rr = RrdpURL rrdpNotifyUri in Right (RrdpU rr, rrdpPP rr)
            | otherwise               -> Left $ UnknownUriType rrdpNotifyUri
        (Nothing, Just repositoryUri) 
            | isRsyncURI repositoryUri -> 
                    case parseRsyncURL (unURI repositoryUri) of 
                        Left e   -> Left $ BrokenUri repositoryUri e
                        Right rr -> Right (RsyncU rr, rsyncPP rr)
            | otherwise                -> Left $ UnknownUriType repositoryUri
        (Nothing, Nothing)             -> Left CertificateDoesntHaveSIA 

publicationPointsFromCertObject :: CerObject -> Either ValidationError (RpkiURL, PublicationPoint)
publicationPointsFromCertObject = publicationPointsFromCert . cwsX509certificate . getCertWithSignature


-- | Get publication points of the certificate.
-- 
getPublicationPointsFromCertObject :: CerObject -> Either ValidationError PublicationPointAccess
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
                            Left e   -> Left $ BrokenUri repositoryUri e
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
    [Change (RsyncHost, RsyncNode FetchStatus Downloadable)]
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
updateStatuses :: Foldable t => PublicationPoints -> t (Repository, FetchStatus) -> PublicationPoints
updateStatuses 
    (PublicationPoints rrdps rsyncs lastSucceded) newStatuses = 
        PublicationPoints 
            (rrdps <> RrdpMap (Map.fromList rrdpUpdates))
            rsyncsUpdates
            (lastSucceded <> EverSucceededMap (Map.fromList lastSuccededUpdates))
    where
        (rrdpUpdates, rsyncsUpdates, lastSuccededUpdates) = 
            foldr foldRepos ([], rsyncs, []) newStatuses

        foldRepos (RrdpR r@RrdpRepository {..}, newStatus) (rrdps', rsyncs', lastS) = 
                    ((uri, r { status = newStatus } :: RrdpRepository) : rrdps', 
                    rsyncs', 
                    status2Success (RrdpU uri) newStatus lastS)

        foldRepos (RsyncR (RsyncRepository (RsyncPublicationPoint uri) _), newStatus) (rrdps', rsyncs', lastS) = 
                    (rrdps', 
                    toRsyncTree uri newStatus rsyncs', 
                    status2Success (RsyncU uri) newStatus lastS)

        status2Success u (FetchedAt _) lastS = (u, AtLeastOnce) : lastS
        status2Success _ _             lastS = lastS
    

everSucceeded :: PublicationPoints -> RpkiURL -> FetchEverSucceeded
everSucceeded PublicationPoints { lastSucceded = EverSucceededMap m } u = 
    fromMaybe Never $ Map.lookup u m

adjustSucceededUrl :: RpkiURL -> PublicationPoints -> PublicationPoints
adjustSucceededUrl u pps =     
    case findPublicationPointStatus pps of 
        Nothing     -> pps
        Just status -> pps & typed %~ succeededFromStatus u status
    where        
        findPublicationPointStatus (PublicationPoints (RrdpMap rrdps) rsyncRepos _) =     
            case u of
                RrdpU  rrdpUrl  -> (^. typed) <$> Map.lookup rrdpUrl rrdps
                RsyncU rsyncUrl -> snd <$> statusInRsyncTree rsyncUrl rsyncRepos  

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

data Downloadable = NotDownloadable | WorthTrying
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

type RsyncNodeNormal = RsyncNode FetchStatus Downloadable

newtype RsyncTree = RsyncTree (Map RsyncHost RsyncNodeNormal)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data RsyncNode a b = Leaf a
               | SubTree { 
                   rsyncChildren :: Map RsyncPathChunk (RsyncNode a b),
                   nodePayload :: b
               } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newRsyncTree :: RsyncTree
newRsyncTree = RsyncTree Map.empty

toRsyncTree :: RsyncURL -> FetchStatus -> RsyncTree -> RsyncTree 
toRsyncTree (RsyncURL host path) fs (RsyncTree byHost) = 
    RsyncTree $ Map.alter (Just . maybe (buildRsyncTree path fs) (pathToRsyncTree path fs)) host byHost    

pathToRsyncTree :: [RsyncPathChunk] -> FetchStatus -> RsyncNodeNormal -> RsyncNodeNormal

pathToRsyncTree [] fs (Leaf fs') = Leaf $ fs' <> fs 
pathToRsyncTree [] fs SubTree {} = Leaf fs 

-- Strange case when we by some reason decide to merge
-- a deeper nested PP while there's a dowloaded  one some
-- higher level. Not supported, don't change the tree.
pathToRsyncTree _ _ (Leaf fs') = Leaf fs'

pathToRsyncTree (u : us) fs subTree@SubTree { rsyncChildren = ch } = 
    case Map.lookup u ch of
        Nothing -> 
            SubTree {
                rsyncChildren = Map.insert u (buildRsyncTree us fs) ch,
                nodePayload  = WorthTrying
            }
        Just child -> subTree { 
                rsyncChildren = Map.insert u (pathToRsyncTree us fs child) ch 
            }

buildRsyncTree :: [RsyncPathChunk] -> FetchStatus -> RsyncNodeNormal
buildRsyncTree [] fs      = Leaf fs
buildRsyncTree (u: us) fs = SubTree {
        rsyncChildren = Map.singleton u $ buildRsyncTree us fs,
        nodePayload  = WorthTrying
    }

statusInRsyncTree :: RsyncURL -> RsyncTree -> Maybe (RsyncURL, FetchStatus)
statusInRsyncTree (RsyncURL host path) (RsyncTree t) = 
    fetchStatus' path [] =<< Map.lookup host t
  where    
    fetchStatus' _ realPath (Leaf fs) = Just (RsyncURL host realPath, fs)
    fetchStatus' [] _  SubTree {} = Nothing
    fetchStatus' (u: us) realPath SubTree {..} = 
        Map.lookup u rsyncChildren >>= fetchStatus' us (realPath <> [u])

rsyncStatuses :: RsyncTree -> [(RsyncURL, FetchStatus)]
rsyncStatuses (RsyncTree hostMap) = 
    mconcat [ statuses (RsyncURL host []) tree 
            | (host, tree) <- Map.toList hostMap ]
  where
    statuses u (Leaf fs) = [(u, fs)]
    statuses (RsyncURL host path) SubTree {..} = 
        mconcat [ statuses (RsyncURL host (path <> [p])) t 
                | (p, t) <- Map.toList rsyncChildren ]        
