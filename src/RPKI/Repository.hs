{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module RPKI.Repository where

import           Codec.Serialise
import           GHC.Generics
import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields

import           Data.X509          (Certificate)

import           Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List          as List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (isJust)
import           Data.Set           (Set)
import qualified Data.Set           as Set

import           RPKI.Domain
import           RPKI.Time
import           RPKI.Errors
import           RPKI.Parse.Parse

import           RPKI.TAL
import           RPKI.Util


data RepositoryFetchType = RRDP | Rsync
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass Serialise

class Fetchable a where
    getURI       :: a -> URI
    getFetchType :: a -> RepositoryFetchType

data FetchStatus = New | FailedAt Instant | FetchedAt Instant
    deriving (Show, Eq, Generic, Serialise)

newtype RsyncPublicationPoint = RsyncPublicationPoint { uri :: URI } 
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass Serialise

data RrdpRepository = RrdpRepository {
        uri      :: !URI,
        rrdpMeta :: !(Maybe (SessionId, Serial)),
        status   :: !FetchStatus
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PublicationPoint = RrdpPP  !RrdpRepository | 
                        RsyncPP !RsyncPublicationPoint
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data Repository = RrdpR !RrdpRepository | 
                  RsyncR !RsyncRepository
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data RsyncRepository = RsyncRepository {
        repos  :: !RsyncPublicationPoint,
        status :: !FetchStatus
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PublicationPoints = PublicationPoints {
    rrdps  :: RrdpMap,
    rsyncs :: RsyncMap
} deriving stock (Show, Eq, Ord, Generic)   

data RsyncParent = ParentURI !URI | Root !FetchStatus
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype RsyncMap = RsyncMap (Map URI RsyncParent)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid)

newtype RrdpMap = RrdpMap (Map URI RrdpRepository)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid)


instance Fetchable PublicationPoint where
    getURI (RrdpPP (RrdpRepository {..})) = uri
    getURI (RsyncPP (RsyncPublicationPoint uri)) = uri
    getFetchType (RrdpPP _)  = RRDP
    getFetchType (RsyncPP _) = Rsync

instance Fetchable Repository where
    getURI (RrdpR (RrdpRepository {..})) = uri
    getURI (RsyncR (RsyncRepository RsyncPublicationPoint {..} _)) = uri
    getFetchType (RrdpR _)  = RRDP
    getFetchType (RsyncR _) = Rsync

-- instance Fetchable URI where
--     getURI = id    
--     getFetchType uri 
--         | 
--     getFetchType (RsyncR _) = Rsync

instance Semigroup PublicationPoints where
    PublicationPoints rrdps1 rsyncs1 <> PublicationPoints rrdps2 rsyncs2 = 
        PublicationPoints (rrdps1 <> rrdps2) (rsyncs1 <> rsyncs2)

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
    compare s1 s2 = compare (timeAndStatus s1) (timeAndStatus s2)
        where 
            -- tuple with 0 and 1 is to take into account a weird 
            -- case of compare (FailedAt t1) (FetchedAt t2) with t1 == t2.
            timeAndStatus New           = (Nothing, 0 :: Int)
            timeAndStatus (FailedAt t)  = (Just t, 0)
            timeAndStatus (FetchedAt t) = (Just t, 1)            

instance Semigroup FetchStatus where
    (<>) = max        

instance Semigroup RsyncMap where
    rs1 <> rs2 = rs1 `mergeRsyncs` rs2

instance Semigroup RrdpMap where
    RrdpMap rs1 <> RrdpMap rs2 = RrdpMap $ Map.unionWith (<>) rs1 rs2        

instance Monoid PublicationPoints where
    mempty = emptyPublicationPoints


rsyncR, rrdpR :: URI -> Repository
rsyncR u = RsyncR $ RsyncRepository (RsyncPublicationPoint u) New
rrdpR u = RrdpR $ RrdpRepository u Nothing New 

rsyncPP, rrdpPP :: URI -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint
rrdpPP u = RrdpPP $ RrdpRepository u Nothing New 

toRepository :: PublicationPoint -> Repository
toRepository (RrdpPP r) = RrdpR r
toRepository (RsyncPP r) = RsyncR $ RsyncRepository r New
  
fetchStatus :: Repository -> FetchStatus 
fetchStatus (RsyncR RsyncRepository {..}) = status
fetchStatus (RrdpR RrdpRepository {..})   = status

toRepoStatusPairs :: PublicationPoints -> [(Repository, FetchStatus)]
toRepoStatusPairs (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncMap)) = 
    rrdpList <> rsyncList
    where 
        rrdpList = map (\r -> (RrdpR r, r ^. typed)) (Map.elems rrdps)
        rsyncList = [ (RsyncR (RsyncRepository (RsyncPublicationPoint u) status), status) | 
                      (u, Root status) <- Map.toList rsyncMap ]


hasURI :: URI -> PublicationPoints -> Bool
hasURI u (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncMap)) = 
    isJust (Map.lookup u rrdps) || isJust (Map.lookup u rsyncMap)


allURIs :: PublicationPoints -> Set URI
allURIs (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs)) = 
    Map.keysSet rrdps <> Map.keysSet rsyncs


findPublicationPointStatus :: URI -> PublicationPoints -> Maybe FetchStatus
findPublicationPointStatus u (PublicationPoints (RrdpMap rrdps) rsyncMap) =     
    case Map.lookup u rrdps of 
        Just RrdpRepository {..} -> Just status
        Nothing                  -> snd <$> findRsyncStatus rsyncMap
    where        
        findRsyncStatus (RsyncMap m) = go u
            where
                go u' =
                    Map.lookup u' m >>= \case            
                        ParentURI parentUri -> go parentUri
                        Root status         -> pure (u, status)


repositoryHierarchy :: PublicationPoints -> 
                    (Map PublicationPoint Repository, 
                     Map Repository (Set PublicationPoint))
repositoryHierarchy (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs)) = 
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
                Root status         -> Just $ RsyncRepository (RsyncPublicationPoint u) status
                ParentURI parentUri -> findRoot parentUri        
                


mergeRsyncs :: RsyncMap -> RsyncMap -> RsyncMap
mergeRsyncs (RsyncMap m1) (RsyncMap m2) = 
    RsyncMap $ Map.foldrWithKey' mergeEach m1 m2
    where
        mergeEach u parentOrStatus m = 
            case Map.splitLookup u m of        
                (_, Just (ParentURI _), _) -> m
                (_, Just (Root status), _) -> 
                    case parentOrStatus of
                        Root status1 -> Map.insert u (Root $ status <> status1) m
                        ParentURI _  -> 
                            -- really weird case, we don't know what to do here,
                            -- but practically just overwrite the PP with a repository
                            Map.insert u (Root status) m
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
    pps & typed %~ (RsyncMap (Map.singleton u (Root New)) <>)

mergeRsyncRepo :: RsyncRepository -> PublicationPoints -> PublicationPoints
mergeRsyncRepo (RsyncRepository (RsyncPublicationPoint u) status) pps = 
    pps & typed %~ (RsyncMap (Map.singleton u (Root status)) <>)

mergeRrdp :: RrdpRepository -> PublicationPoints -> PublicationPoints
mergeRrdp r@RrdpRepository { uri = u } pps = 
    case Map.lookup u (pps ^. rrdpLens) of
        Nothing                             -> pps & rrdpLens %~ (Map.insert u r)
        Just existing@RrdpRepository { .. } -> pps & rrdpLens %~ inserted
            where 
                inserted m = if r == existing -- most often case
                                then m 
                                else Map.insert u existing m
    where 
        rrdpLens = typed @RrdpMap . coerced    


-- | Merge rsync repositories into the hierarchy based on their URIs
-- e.g.
--   rsync://host/foo/
--     rsync://host/foo/bar/
--       rsync://host/foo/bar/quux
--     rsync://host/foo/baz/
--       rsync://host/foo/baz/nup
--       rsync://host/foo/baz/bop
--
fromRsyncPPs :: [RsyncPublicationPoint] -> PublicationPoints
fromRsyncPPs = List.foldr mergeRsyncPP mempty

emptyPublicationPoints :: PublicationPoints
emptyPublicationPoints = PublicationPoints mempty mempty


-- | Update repository and return the status of the repository 
-- | as it was before the update.
mergePP :: PublicationPoint -> PublicationPoints -> PublicationPoints
mergePP (RrdpPP r) = mergeRrdp r
mergePP (RsyncPP r) = mergeRsyncPP r    

mergeRepo :: Repository -> PublicationPoints -> PublicationPoints
mergeRepo (RrdpR r) = mergeRrdp r
mergeRepo (RsyncR r) = mergeRsyncRepo r    

mergeRepos :: Foldable t =>  t Repository -> PublicationPoints -> PublicationPoints
mergeRepos repos pps = foldr mergeRepo pps repos


-- | Extract repositories from URIs in TAL and in TA certificate,
-- | use some reasonable heuristics, but don't try to be very smart.
-- | Prefer RRDP to rsync for everything.
-- | URI of the repository is supposed to be a "real" one, i.e. where
-- | repository can actually be downloaded from.
createRepositoriesFromTAL :: TAL -> CerObject -> Either ValidationError (NonEmpty Repository)
createRepositoriesFromTAL tal (cwsX509certificate . getCertWithSignature -> cert) = 
    case tal of 
        PropertiesTAL {..} -> do 
            (certUri, publicationPoint) <- publicationPointsFromCert cert
            uniquePrefetchRepos   <- mapM 
                (fmap snd . fromURI) $ filter (/= certUri) prefetchUris

            let prefetchReposToUse = 
                    case [ r | r@(RrdpR _) <- uniquePrefetchRepos ] of
                        []    -> uniquePrefetchRepos
                        rrdps -> rrdps

            pure $ toRepository publicationPoint :| prefetchReposToUse 

        RFC_TAL {..} -> 
            (\(_, r) -> toRepository r :| []) <$> publicationPointsFromCert cert            
    where        
        fromURI u = case () of 
            _   | isRrdpURI u  -> Right (u, rrdpR u)            
                | isRsyncURI u -> Right (u, rsyncR u)
                | otherwise    -> Left $ UnknownUriType u
        

-- | Create repository from the publication points of the certificate.
publicationPointsFromCert :: Certificate -> Either ValidationError (URI, PublicationPoint)
publicationPointsFromCert cert = 
    case (getRrdpNotifyUri cert, getRepositoryUri cert) of
        (Just rrdpNotifyUri, _) 
            | isRrdpURI rrdpNotifyUri -> Right (rrdpNotifyUri, rrdpPP rrdpNotifyUri)
            | otherwise               -> Left $ UnknownUriType rrdpNotifyUri
        (Nothing, Just repositoryUri) 
            | isRsyncURI repositoryUri -> Right (repositoryUri, rsyncPP repositoryUri)
            | otherwise                -> Left $ UnknownUriType repositoryUri
        (Nothing, Nothing)             -> Left CertificateDoesn'tHaveSIA 

publicationPointsFromCertObject :: CerObject -> Either ValidationError (URI, PublicationPoint)
publicationPointsFromCertObject = publicationPointsFromCert . cwsX509certificate . getCertWithSignature


data Change a = Put a | Remove a 
    deriving stock (Show, Eq, Ord, Generic)

-- | Derive a diff between two states of publication points
changeSet :: PublicationPoints -> PublicationPoints -> 
            ([Change RrdpRepository], [Change (URI, RsyncParent)])
changeSet 
    (PublicationPoints (RrdpMap rrdpOld) (RsyncMap rsyncOld)) 
    (PublicationPoints (RrdpMap rrdpNew) (RsyncMap rsyncNew)) = 
    (putNewRrdps <> removeOldRrdps, putNewRsyncs <> removeOldRsyncs)
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


updateStatuses :: Foldable t => PublicationPoints -> t (Repository, FetchStatus) -> PublicationPoints
updateStatuses 
    (PublicationPoints rrdps rsyncs) newStatuses = 
        PublicationPoints 
            (rrdps <> RrdpMap (Map.fromList rrdpUpdates))
            (rsyncs <> RsyncMap (Map.fromList rsyncUpdates))
    where
        (rrdpUpdates, rsyncUpdates) = foldr foldRepos ([], []) newStatuses

        foldRepos (RrdpR r@RrdpRepository {..}, newStatus) (rrdps', rsyncs') = 
                    ((uri, r { status = newStatus } :: RrdpRepository) : rrdps', rsyncs')
        foldRepos (RsyncR (RsyncRepository (RsyncPublicationPoint uri) _), newStatus) (rrdps', rsyncs') = 
                    (rrdps', (uri, Root newStatus) : rsyncs')


-- Limit PublicationPoints only to the set of URIs in the set that comes the first argument.
-- For rsync, also add all the parent URLs.
shrinkTo :: PublicationPoints -> Set URI -> PublicationPoints
shrinkTo (PublicationPoints (RrdpMap rrdps) (RsyncMap rsyncs)) uris = 
    PublicationPoints (RrdpMap rrdps') (RsyncMap rsyncs')
    where
        rrdps'  = Map.filterWithKey (\u _ -> u `Set.member` uris) rrdps
        rsyncs' = Map.foldrWithKey addWithParents Map.empty rsyncs

        addWithParents u parent m 
            | u `Set.member` uris = go u parent
            | otherwise           = m 
            where 
                go u' r@(Root _)       = Map.insert u' r m
                go u' p@(ParentURI pu) = Map.insert u' p m <> Map.fromList (gatherPathToRoot pu)

                gatherPathToRoot u' = 
                    case Map.lookup u' rsyncs of 
                        Nothing             -> []
                        Just (ParentURI pu) -> (u', ParentURI pu) : gatherPathToRoot pu
                        Just r@(Root _)     -> [(u', r)]
