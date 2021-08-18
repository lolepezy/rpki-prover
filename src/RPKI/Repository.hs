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

import           GHC.Generics

import           Data.Generics.Labels
import           Data.Generics.Product.Typed
import           Data.Ord

import           Data.X509                   (Certificate)

import           Data.List.NonEmpty          (NonEmpty (..), nonEmpty)

import qualified Data.List                   as List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Monoid.Generic
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Time

import           Data.Semigroup
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
        rsyncs :: RsyncMap,
        lastSucceded :: EverSucceededMap
    } 
    deriving stock (Show, Eq, Ord, Generic)   
    deriving Semigroup via GenericSemigroup PublicationPoints   
    deriving Monoid    via GenericMonoid PublicationPoints

data RsyncParent = ParentURI RsyncURL | Root FetchStatus
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype RsyncMap = RsyncMap { unRsyncMap :: Map RsyncURL RsyncParent }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid)

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

instance Semigroup RsyncMap where
    (<>) = mergeRsyncs

instance Semigroup EverSucceededMap where
    EverSucceededMap ls1 <> EverSucceededMap ls2 = EverSucceededMap $ Map.unionWith (<>) ls1 ls2        

instance Semigroup RrdpMap where
    RrdpMap rs1 <> RrdpMap rs2 = RrdpMap $ Map.unionWith (<>) rs1 rs2        
    
getFetchStatus :: Repository -> FetchStatus
getFetchStatus (RrdpR r)  = r ^. #status
getFetchStatus (RsyncR r) = r ^. #status

newRepositoryProcessing :: STM RepositoryProcessing
newRepositoryProcessing = RepositoryProcessing <$> 
        newTVar mempty <*> 
        newTVar mempty <*>          
        newTVar mempty <*>          
        newTVar mempty 

newRepositoryProcessingIO :: IO RepositoryProcessing
newRepositoryProcessingIO = atomically newRepositoryProcessing

rsyncR :: RsyncURL -> Repository
rrdpR  :: RrdpURL  -> Repository
rsyncR u = RsyncR $ RsyncRepository (RsyncPublicationPoint u) Pending
rrdpR u = RrdpR $ RrdpRepository u Nothing Pending

rsyncPP :: RsyncURL -> PublicationPoint
rrdpPP  :: RrdpURL  -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint
rrdpPP u = RrdpPP $ RrdpRepository u Nothing Pending

toRepository :: PublicationPoint -> Repository
toRepository (RrdpPP r) = RrdpR r
toRepository (RsyncPP r) = RsyncR $ RsyncRepository r Pending
  
fetchStatus :: Repository -> FetchStatus 
fetchStatus (RsyncR RsyncRepository {..}) = status
fetchStatus (RrdpR RrdpRepository {..})   = status

toRepoStatusPairs :: PublicationPoints -> [(Repository, FetchStatus)]
toRepoStatusPairs (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncMap) _) = 
    rrdpList <> rsyncList
  where 
    rrdpList = map (\r -> (RrdpR r, r ^. typed)) (Map.elems rrdps)
    rsyncList = [ (RsyncR (RsyncRepository (RsyncPublicationPoint u) status), status) | 
                    (u, Root status) <- Map.toList rsyncMap ]


hasURI :: RpkiURL -> PublicationPoints -> Bool
hasURI u (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncMap) _) = 
    case u of
        RrdpU  rrdpUrl  -> isJust $ Map.lookup rrdpUrl rrdps
        RsyncU rsyncUrl -> isJust $ Map.lookup rsyncUrl rsyncMap    


allURIs :: PublicationPoints -> Set RpkiURL
allURIs (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs) _) = 
    Map.foldMapWithKey (\u _ -> Set.singleton $ RrdpU u) rrdps <> 
    Map.foldMapWithKey (\u _ -> Set.singleton $ RsyncU u) rsyncs


findPublicationPointStatus :: RpkiURL -> PublicationPoints -> Maybe FetchStatus
findPublicationPointStatus u (PublicationPoints (RrdpMap rrdps) rsyncMap _) =     
    case u of
        RrdpU  rrdpUrl  -> (^. typed) <$> Map.lookup rrdpUrl rrdps
        RsyncU rsyncUrl -> snd <$> findRsyncStatus rsyncMap rsyncUrl           
  where        
    findRsyncStatus (RsyncMap m) rsyncUrl = go rsyncUrl
      where
        go u' =
            Map.lookup u' m >>= \case            
                ParentURI parentUri -> go parentUri
                Root status         -> pure (u, status)


repositoryHierarchy :: PublicationPoints -> 
                    (Map PublicationPoint Repository, 
                     Map Repository (Set PublicationPoint))
repositoryHierarchy (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs) _) = 
    (direct, inverse)    
  where
    direct = rrdp <> rsync    
    inverse = Map.foldrWithKey 
        (\pp root m -> m <> Map.singleton root (Set.singleton pp)) 
        Map.empty 
        direct

    rrdp = Map.fromList $ map (\r -> (RrdpPP r, RrdpR r)) $ Map.elems rrdps        
    rsync = Map.fromList [ 
                (RsyncPP (RsyncPublicationPoint u), RsyncR root) | 
                    (u, Just root) <- map (\u -> (u, findRoot u)) $ Map.keys rsyncs 
            ]
    findRoot u = 
        Map.lookup u rsyncs >>= \case             
            Root status          -> Just $ RsyncRepository (RsyncPublicationPoint u) status
            ParentURI parentUri  -> findRoot parentUri        
                

repositoryFromPP :: PublicationPoints -> RpkiURL -> Maybe Repository                    
repositoryFromPP (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs) _) rpkiUrl = 
    case rpkiUrl of
        RrdpU u  -> RrdpR <$> Map.lookup u rrdps
        RsyncU u -> RsyncR <$> findRoot u    
  where    
    findRoot u = 
        Map.lookup u rsyncs >>= \case             
            Root status          -> Just $ RsyncRepository (RsyncPublicationPoint u) status
            ParentURI parentUri  -> findRoot parentUri  


-- | Merge two rsync repository maps
-- It's a commutative and associative operation
mergeRsyncs :: RsyncMap -> RsyncMap -> RsyncMap
mergeRsyncs (RsyncMap m1) (RsyncMap m2) = 
    RsyncMap $ Map.foldrWithKey' mergeEach m1 m2
  where
    mergeEach u parentOrStatus m = 
        case Map.splitLookup u m of        
            (_, Just (ParentURI _), _) -> m
            (_, Just r@(Root status), _) -> 
                case parentOrStatus of
                    Root status1 -> 
                        Map.insert u (Root (status <> status1)) m
                    ParentURI _  -> 
                        -- really weird case, we don't know what to do here,
                        -- but practically just overwrite the PP with a repository
                        Map.insert u r m

            (potentialParents, _, potentialChildren) -> 
                replaceTheirParents $ Map.insert u parentUriOrStatus m
              where 
                parentUriOrStatus = go potentialParents                            
                  where
                    go low = 
                        case Map.maxViewWithKey low of 
                            Nothing                               -> parentOrStatus
                            Just ((longestParentUri, _), evenLower)
                                | longestParentUri `isParentOf` u -> ParentURI longestParentUri
                                | otherwise                       -> go evenLower

                replaceTheirParents = go potentialChildren
                  where
                    go up resultMap = 
                        case Map.minViewWithKey up of 
                            Nothing -> resultMap
                            Just ((possiblyChild, something), moreChildren)
                                | u `isParentOf` possiblyChild -> 
                                    case something of
                                        ParentURI itsParentUri 
                                            | itsParentUri `isParentOf` u -> 
                                                go moreChildren $ Map.insert possiblyChild (ParentURI u) resultMap
                                            | otherwise                   -> go moreChildren resultMap
                                        Root _                            -> 
                                                go moreChildren $ Map.insert possiblyChild (ParentURI u) resultMap
                                | otherwise                               -> resultMap            


mergeRsyncPP :: RsyncPublicationPoint -> PublicationPoints -> PublicationPoints
mergeRsyncPP (RsyncPublicationPoint u) pps = 
    pps & typed %~ (RsyncMap (Map.singleton u (Root Pending)) <>)

mergeRsyncRepo :: RsyncRepository -> PublicationPoints -> PublicationPoints
mergeRsyncRepo 
    (RsyncRepository (RsyncPublicationPoint u) status) 
    (PublicationPoints rrdps rsyncs lastSucceded) = 
    PublicationPoints rrdps rsyncs' lastSucceded'
  where
    rsyncs'       = rsyncs <> RsyncMap (Map.singleton u (Root status)) 
    lastSucceded' = succeededFromStatus (RsyncU u) status lastSucceded
    

mergeRrdp :: RrdpRepository -> PublicationPoints -> PublicationPoints
mergeRrdp r@RrdpRepository { .. } 
        (PublicationPoints (RrdpMap rrdps) rsyncs lastSucceded) =
    PublicationPoints (RrdpMap rrdps') rsyncs lastSucceded'
  where
    rrdps' = case Map.lookup uri rrdps of
            Nothing       -> Map.insert uri r rrdps
            Just existing -> if r == existing
                                then  rrdps 
                                else Map.insert uri existing  rrdps                    
    lastSucceded' = succeededFromStatus (RrdpU uri) status lastSucceded        


succeededFromStatus :: RpkiURL -> FetchStatus -> EverSucceededMap -> EverSucceededMap
succeededFromStatus u (FetchedAt t) lastSucceded = 
    lastSucceded <> EverSucceededMap (Map.singleton u AtLeastOnce)
succeededFromStatus _ _ lastSucceded = lastSucceded


-- | Merge rsync publication points into the hierarchy based on their URIs
-- e.g.
--   rsync://host/foo/
--     rsync://host/foo/bar/
--       rsync://host/foo/bar/quux
--     rsync://host/foo/baz/
--       rsync://host/foo/baz/nup
--       rsync://host/foo/baz/bop
--
fromRsyncPPs :: [RsyncPublicationPoint] -> PublicationPoints
fromRsyncPPs = List.foldr mergeRsyncPP emptyPublicationPoints

emptyPublicationPoints :: PublicationPoints
emptyPublicationPoints = PublicationPoints mempty mempty mempty

mergePP :: PublicationPoint -> PublicationPoints -> PublicationPoints
mergePP (RrdpPP r) = mergeRrdp r
mergePP (RsyncPP r) = mergeRsyncPP r    

mergeRepo :: Repository -> PublicationPoints -> PublicationPoints
mergeRepo (RrdpR r) = mergeRrdp r
mergeRepo (RsyncR r) = mergeRsyncRepo r    

mergeRepos :: Foldable t => t Repository -> PublicationPoints -> PublicationPoints
mergeRepos repos pps = foldr mergeRepo pps repos

mergePPs :: Foldable t => t Repository -> PublicationPoints -> PublicationPoints
mergePPs repos pps = foldr mergeRepo pps repos


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

        RFC_TAL {..} -> do 
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
            | isRsyncURI repositoryUri -> let rr = RsyncURL repositoryUri in Right (RsyncU rr, rsyncPP rr)
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
                    | isRsyncURI repositoryUri -> Right [rsyncPP $ RsyncURL repositoryUri]
                    | otherwise                -> Left $ UnknownUriType repositoryUri
                Nothing -> Right []

    case nonEmpty (rrdp <> rsync) of 
        Nothing -> Left CertificateDoesntHaveSIA
        Just ne -> Right $ PublicationPointAccess ne
    

data Change a = Put a | Remove a 
    deriving stock (Show, Eq, Ord, Generic)

data ChangeSet = ChangeSet
    [Change RrdpRepository]    
    [Change (RsyncURL, RsyncParent)]
    [Change (RpkiURL, FetchEverSucceeded)]


-- | Derive a diff between two states of publication points
changeSet :: PublicationPoints -> PublicationPoints -> ChangeSet
changeSet 
    (PublicationPoints (RrdpMap rrdpOld) (RsyncMap rsyncOld) (EverSucceededMap lastSuccededOld)) 
    (PublicationPoints (RrdpMap rrdpNew) (RsyncMap rsyncNew) (EverSucceededMap lastSuccededNew)) = 
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


-- Update statuses of the repositories and last successful fetch times for the 
updateStatuses :: Foldable t => PublicationPoints -> t (Repository, FetchStatus) -> PublicationPoints
updateStatuses 
    (PublicationPoints rrdps rsyncs lastSucceded) newStatuses = 
        PublicationPoints 
            (rrdps <> RrdpMap (Map.fromList rrdpUpdates))
            (rsyncs <> RsyncMap (Map.fromList rsyncUpdates))
            (lastSucceded <> EverSucceededMap (Map.fromList lastSuccededUpdates))
    where
        (rrdpUpdates, rsyncUpdates, lastSuccededUpdates) = 
            foldr foldRepos ([], [], []) newStatuses

        foldRepos (RrdpR r@RrdpRepository {..}, newStatus) (rrdps', rsyncs', lastS) = 
                    ((uri, r { status = newStatus } :: RrdpRepository) : rrdps', 
                    rsyncs', 
                    status2Success (RrdpU uri) newStatus lastS)
        foldRepos (RsyncR (RsyncRepository (RsyncPublicationPoint uri) _), newStatus) (rrdps', rsyncs', lastS) = 
                    (rrdps', 
                    (uri, Root newStatus) : rsyncs', 
                    status2Success (RsyncU uri) newStatus lastS)

        status2Success u (FetchedAt t) lastS = (u, AtLeastOnce) : lastS
        status2Success _ _             lastS = lastS


-- Limit PublicationPoints only to the set of URIs in the set that comes the first argument.
-- For rsync, also add all the parent URLs.
shrinkTo :: PublicationPoints -> Set RpkiURL -> PublicationPoints
shrinkTo (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs) (EverSucceededMap lastSucceded)) uris = 
    PublicationPoints (RrdpMap rrdps') (RsyncMap rsyncs') (EverSucceededMap lastSucceded')
    where
        rrdps'        = Map.filterWithKey (\u _ -> u `Set.member` rrdpURLs) rrdps
        rsyncs'       = Map.foldrWithKey addWithParents Map.empty rsyncs
        lastSucceded' = Map.filterWithKey (\u _ -> u `Set.member` uris) lastSucceded

        (rrdpURLs, rsyncURLs) = bimap Set.fromList Set.fromList 
                                    $ Set.foldr partitionURLs ([], []) uris
        partitionURLs (RrdpU r) (rr, rs)  = (r : rr, rs)
        partitionURLs (RsyncU r) (rr, rs) = (rr, r : rs)

        addWithParents u parent m 
            | u `Set.member` rsyncURLs = go u parent
            | otherwise                = m 
            where 
                go u' r@(Root _)       = Map.insert u' r m
                go u' p@(ParentURI pu) = Map.insert u' p m <> Map.fromList (gatherPathToRoot pu)

                gatherPathToRoot u' = 
                    case Map.lookup u' rsyncs of 
                        Nothing             -> []
                        Just (ParentURI pu) -> (u', ParentURI pu) : gatherPathToRoot pu
                        Just r@(Root _)     -> [(u', r)]


-- | Fix lastSucceeded map based on statuses. 
-- 
-- We need to have this because 
-- 
-- * PublicationPoints is a semigroup for convenience of using <> and mconcat for 
-- multiple things (maps, statuses, etc.);
-- 
-- * lastSucceeded is semantically dependent on statuses (stored in RrrdpMap and RsyncMap);
-- 
-- * It's impossible to adjust lastSucceeded based on RrrdpMap and RsyncMap while keeping
-- semigroup property.
--
-- That's why adjusting lastSucceeded must happen as a separate step without all the semigroup laws.
--
-- There must be a more elegant way of doing it, but this works for now.
-- 
adjustLastSucceeded :: PublicationPoints -> PublicationPoints
adjustLastSucceeded 
    (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs) lastSucceded) = 
    PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs) lastSucceded'
    where        
        lastSucceded' = lastSucceded <> rrdpSucceded <> rsyncSucceded

        rrdpSucceded = Map.foldrWithKey 
            (\_ RrdpRepository {..} ls -> succeededFromStatus (RrdpU uri) status ls) 
            mempty rrdps

        rsyncSucceded = foldr 
            (\(u, status) ls -> succeededFromStatus (RsyncU u) status ls) 
            mempty
            [ (u, status) | (u, Root status) <- Map.toList rsyncs ]        


everSucceeded :: PublicationPoints -> RpkiURL -> FetchEverSucceeded
everSucceeded PublicationPoints { lastSucceded = EverSucceededMap m } u = 
    fromMaybe Never $ Map.lookup u m


adjustSucceededUrl :: RpkiURL -> PublicationPoints -> PublicationPoints
adjustSucceededUrl u pps =     
    case findPublicationPointStatus u pps of 
        Nothing     -> pps
        Just status -> pps & typed %~ succeededFromStatus u status

