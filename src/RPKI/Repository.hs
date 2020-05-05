{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Repository where

import           Codec.Serialise
import           GHC.Generics

import           Data.X509          (Certificate)

import           Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List          as List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Data.Text          as Text

import           Data.Hourglass     (DateTime)

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Parse.Parse

import           RPKI.TAL


data RepositoryStatus = New | FailedAt DateTime | FetchedAt DateTime
    deriving (Show, Eq, Ord, Generic, Serialise)

newtype RsyncPublicationPoint = RsyncPublicationPoint { uri :: URI } 
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass Serialise

data RrdpRepository = RrdpRepository {
        uri      :: !URI,
        rrdpMeta :: !(Maybe (SessionId, Serial)),
        status   :: !RepositoryStatus
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PublicationPoint = RrdpPP !RrdpRepository | 
                        RsyncPP !RsyncPublicationPoint
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data Repository = RrdpR !RrdpRepository | 
                  RsyncR !RsyncRepository
    deriving (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data RsyncRepository = RsyncRepository {
        repos  :: !RsyncPublicationPoint,
        status :: !RepositoryStatus
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PublicationPoints = PublicationPoints {
    rrdps  :: Map URI RrdpRepository,
    rsyncs :: RsyncMap
} deriving stock (Show, Eq, Ord, Generic)   

data RsyncParent = ParentURI !URI | Root !RepositoryStatus
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype RsyncMap = RsyncMap (Map URI RsyncParent)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise


rsyncR, rrdpR :: URI -> Repository
rsyncR u = RsyncR $ RsyncRepository (RsyncPublicationPoint u) New
rrdpR u = RrdpR $ RrdpRepository u Nothing New 

rsyncPP, rrdpPP :: URI -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint
rrdpPP u = RrdpPP $ RrdpRepository u Nothing New 

toRepository :: PublicationPoint -> Repository
toRepository (RrdpPP r) = RrdpR r
toRepository (RsyncPP r) = RsyncR $ RsyncRepository r New

repositoryURI :: Repository -> URI
repositoryURI (RrdpR RrdpRepository {..}) = uri
repositoryURI (RsyncR (RsyncRepository RsyncPublicationPoint {..} _)) = uri

publicationPointURI :: PublicationPoint -> URI
publicationPointURI (RrdpPP RrdpRepository {..}) = uri
publicationPointURI (RsyncPP RsyncPublicationPoint {..}) = uri
  
repositoryStatus :: Repository -> RepositoryStatus 
repositoryStatus (RsyncR RsyncRepository {..}) = status
repositoryStatus (RrdpR RrdpRepository {..})   = status

rsyncRepositories :: RsyncMap -> [RsyncRepository]
rsyncRepositories (RsyncMap rsyncMap) = 
    [ RsyncRepository (RsyncPublicationPoint u) status | (u, Root status) <- Map.toList rsyncMap ]

repositories :: PublicationPoints -> Map URI Repository
repositories (PublicationPoints rrdps (RsyncMap rsyncMap)) = 
    Map.map RrdpR rrdps <>
    Map.fromList [ 
        (u, RsyncR (RsyncRepository (RsyncPublicationPoint u) status)) | 
        (u, Root status) <- Map.toList rsyncMap ]



repositoryHierarchy :: PublicationPoints -> 
                    (Map PublicationPoint Repository, Map Repository (Set PublicationPoint))
repositoryHierarchy (PublicationPoints rrdps (RsyncMap rsyncs)) = (direct, inverse)    
    where
        direct = rrdp <> rsync    
        inverse = Map.foldrWithKey 
            (\pp root m -> m <> Map.singleton root (Set.singleton pp)) 
            Map.empty 
            direct

        rrdp = Map.fromList $ map (\r -> (RrdpPP r, RrdpR r)) $ Map.elems rrdps        
        rsync = Map.fromList $ [ 
                    (RsyncPP (RsyncPublicationPoint u), RsyncR root) | 
                        (u, Just root) <- map (\u -> (u, findRoot u)) $ Map.keys rsyncs 
                ]
        findRoot u = 
            Map.lookup u rsyncs >>= \case             
                Root status         -> Just $ RsyncRepository (RsyncPublicationPoint u) status
                ParentURI parentUri -> findRoot parentUri        
                


findRepositoryStatus :: URI -> RsyncMap -> Maybe (URI, RepositoryStatus)
findRepositoryStatus u (RsyncMap m) = go u
    where
        go u' =
            Map.lookup u' m >>= \case            
                ParentURI parentUri -> go parentUri
                Root status   -> pure (u, status)


-- | Update repository status and return the old one 
-- | if the uri is pointing to the rsync root.
updateRepositoryStatus :: URI -> RsyncMap -> RepositoryStatus -> 
                        (RsyncMap, Maybe RepositoryStatus)
updateRepositoryStatus u rm@(RsyncMap m) status = 
    case Map.lookup u m of
        Nothing            -> (rm, Nothing)
        Just (ParentURI _) -> (rm, Nothing)
        Just (Root s)      -> (RsyncMap $ Map.insert u (Root status) m, Just s)    


-- | Merge rsync repositories into the hierarchy based on their URIs
-- e.g.
--   rsync://host/foo/
--     rsync://host/foo/bar/
--       rsync://host/foo/bar/quux
--     rsync://host/foo/baz/
--       rsync://host/foo/baz/nup
--       rsync://host/foo/baz/bop
--
merge :: URI -> RsyncMap -> (RsyncMap, RepositoryStatus)
merge u rsyncMap@(RsyncMap m) = 
    case Map.splitLookup u m of        
        (_, Just (ParentURI parentUri), _) -> (rsyncMap, status)
            where
                Just (_, status) = findRepositoryStatus parentUri rsyncMap

        (_, Just (Root status), _) -> (rsyncMap, status)

        (potentialParents, _, potentialChildren) -> (RsyncMap m', New)
            where
                parentUriOrStatus = go potentialParents
                    where 
                        go low = 
                            case Map.maxViewWithKey low of 
                                Nothing                               -> Root New
                                Just ((longestParentUri, _), evenLower)
                                    | longestParentUri `isParentOf` u -> ParentURI longestParentUri
                                    | otherwise                       -> go evenLower

                m' = replaceTheirParents $ Map.insert u parentUriOrStatus m

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
                                                | otherwise                   -> go moreChildren $ resultMap
                                            Root _                            -> 
                                                    go moreChildren $ Map.insert possiblyChild (ParentURI u) resultMap
                                    | otherwise                               -> resultMap            


createRsyncMap :: [RsyncPublicationPoint] -> RsyncMap
createRsyncMap = List.foldr (\(RsyncPublicationPoint u) rm -> fst $ u `merge` rm) (RsyncMap Map.empty)

emptyRepositories :: PublicationPoints
emptyRepositories = PublicationPoints Map.empty $ RsyncMap Map.empty 


-- | Update repository and return the status of the repository 
-- | as it was before the update.
updatePublicationPoints :: PublicationPoints -> PublicationPoint -> (PublicationPoints, RepositoryStatus)
updatePublicationPoints PublicationPoints {..} repository =
    case repository of  
        RrdpPP r@RrdpRepository { uri = u } -> 
            case Map.lookup u rrdps of
                Nothing -> (PublicationPoints rrdps' rsyncs, New)
                    where 
                        rrdps' = Map.insert u r rrdps
                Just existing@RrdpRepository { status = s } -> (PublicationPoints rrdps' rsyncs, s)
                    where 
                        rrdps' = if r == existing -- most often case
                                then rrdps 
                                else Map.insert u existing rrdps

        RsyncPP (RsyncPublicationPoint u) ->              
            (PublicationPoints rrdps rsyncs', status) 
            where
                (rsyncs', status) = u `merge` rsyncs    



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
                (\u -> snd <$> fromURI u) $ filter (/= certUri) prefetchUris

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
        

isRsyncURI, isRrdpURI :: URI -> Bool
isRsyncURI (URI u) = "rsync://" `Text.isPrefixOf` u
isRrdpURI (URI u) = "http://" `Text.isPrefixOf` u || "https://" `Text.isPrefixOf` u                        

isParentOf :: URI -> URI -> Bool
isParentOf (URI p) (URI c) = p `Text.isPrefixOf` c


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


-- | Derive a change set to apply to the 
changeSet :: PublicationPoints -> PublicationPoints -> 
            ([Change RrdpRepository], [Change (URI, RsyncParent)])
changeSet 
    (PublicationPoints rrdpOld (RsyncMap rsyncOld)) 
    (PublicationPoints rrdpNew (RsyncMap rsyncNew)) = 
    (putNewRrdps <> removeOldRrdps, putNewRsyncs <> removeOldRsyncs)
    where
        rrdpOldSet = Set.fromList $ Map.elems rrdpOld
        rrdpNewSet = Set.fromList $ Map.elems rrdpNew
        putNewRrdps = map Put $ filter (not . (`Set.member` rrdpOldSet)) $ Map.elems rrdpNew
        removeOldRrdps = map Remove $ filter (not . (`Set.member` rrdpNewSet)) $ Map.elems rrdpOld

        rsyncOldList = Map.toList rsyncOld
        rsyncNewList = Map.toList rsyncNew
        rsyncOldSet = Set.fromList rsyncOldList
        rsyncNewSet = Set.fromList $ Map.toList rsyncNew
        putNewRsyncs = map Put $ filter (not . (`Set.member` rsyncOldSet)) rsyncNewList
        removeOldRsyncs = map Remove $ filter (not . (`Set.member` rsyncNewSet)) rsyncOldList


-- | Derive a change set to apply to the 
changeSetFromMap :: Map URI Repository -> Map URI Repository -> 
                    ([Change RrdpRepository], [Change (URI, RsyncParent)])
changeSetFromMap oldRepos newRepos = 
    (putNewRrdps <> removeOldRrdps, putNewRsyncs <> removeOldRsyncs)
    where
        oldRepoList = Map.toList oldRepos
        newRepoList = Map.toList newRepos

        putNewRrdps    = [ Put r    | (u, rr@(RrdpR r)) <- newRepoList, Map.lookup u oldRepos /= Just rr ]
        removeOldRrdps = [ Remove r | (u, rr@(RrdpR r)) <- oldRepoList, Map.lookup u newRepos /= Just rr ]

        putNewRsyncs    = [ Put (u, Root status) | 
            (u, r@(RsyncR (RsyncRepository _ status)) ) <- newRepoList, Map.lookup u oldRepos /= Just r ]

        removeOldRsyncs = [ Remove (u, Root status) | 
            (u, r@(RsyncR (RsyncRepository _ status))) <- newRepoList, Map.lookup u oldRepos /= Just r ]


updateStatuses :: Foldable t => PublicationPoints -> t (Repository, RepositoryStatus) -> PublicationPoints
updateStatuses 
    (PublicationPoints rrdps (RsyncMap rsyncs)) newStatuses = PublicationPoints rrdps' rsyncs'
    where
        rrdps' = foldr foldRrdp rrdps newStatuses
            where 
                foldRrdp (RrdpR r@RrdpRepository { uri = u }, newStatus) m = let 
                    r' :: RrdpRepository = r { status = newStatus }
                    in Map.insert u r' m
                foldRrdp (RsyncR _, _) m = m

        rsyncs' = RsyncMap $ foldr foldRsync rsyncs newStatuses
            where 
                foldRsync (RrdpR _, _) m = m
                foldRsync (RsyncR (RsyncRepository (RsyncPublicationPoint u) _), newStatus) m = 
                    case Map.lookup u m of 
                        Just (Root _) -> Map.insert u (Root newStatus) m
                        _             -> m                    


{- 

let rrdp1 = RrdpRepository (URI "u1") Nothing New
let rrdp2 = RrdpRepository (URI "u2") Nothing New

let rrdps = M.fromList $ Prelude.map (\r@RrdpRepository {..} -> (uri, r)  ) [rrdp1, rrdp2]

let rsyncs = M.fromList [ (URI "u1/path/a", ParentURI (URI "u1/path")),  ( URI "u1/path", Root New)  ]


-}