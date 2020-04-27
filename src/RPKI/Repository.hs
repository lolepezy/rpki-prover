{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Repository where

import           Codec.Serialise
import           Control.Monad
import           GHC.Generics

import           Data.X509          (Certificate)

import           Data.List.NonEmpty (NonEmpty (..))

import Data.Bifunctor

import qualified Data.List          as List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe
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


data RsyncTree = RsyncTree RsyncPublicationPoint [RsyncTree]
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Forest = Forest [RsyncTree]
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PublicationPoints = PublicationPoints {
    rrdps  :: Map URI RrdpRepository,
    rsyncs :: RsyncMap
} deriving stock (Show, Eq, Ord, Generic)   


rsyncR :: URI -> Repository
rsyncR u = RsyncR $ RsyncRepository (RsyncPublicationPoint u) New

rrdpR :: URI -> Repository
rrdpR u = RrdpR $ RrdpRepository u Nothing New 

rsyncPP :: URI -> PublicationPoint
rsyncPP = RsyncPP . RsyncPublicationPoint

rrdpPP :: URI -> PublicationPoint
rrdpPP u = RrdpPP $ RrdpRepository u Nothing New 

toRepository :: PublicationPoint -> Repository
toRepository (RrdpPP r) = RrdpR r
toRepository (RsyncPP r) = RsyncR $ RsyncRepository r New


repositoryURI :: Repository -> URI
repositoryURI (RrdpR (RrdpRepository {..})) = uri
repositoryURI (RsyncR (RsyncRepository (RsyncPublicationPoint {..}) _)) = uri


-- | Merge rsync repositories into the hierarchy based on their URIs
-- e.g.
--   rsync://host/foo/
--     rsync://host/foo/bar/
--       rsync://host/foo/bar/quux
--     rsync://host/foo/baz/
--       rsync://host/foo/baz/nup
--       rsync://host/foo/baz/bop
--
mergeIntoTrees :: RsyncPublicationPoint -> [RsyncTree] -> ([RsyncTree], RepositoriesUpdated RsyncTree)
mergeIntoTrees r [] = ([RsyncTree r []], AddedNew)
mergeIntoTrees 
    r@(RsyncPublicationPoint newUri) 
    trees@(tree@(RsyncTree maybeParent@(RsyncPublicationPoint uri) children) : rts) = 
        first List.sort $ 
            if uri == newUri 
                then (trees, AlreadyThere tree)
                else if uri `isParentOf` newUri
                    then let 
                        (merged, update) = r `mergeIntoTrees` children
                        tree' = RsyncTree maybeParent merged : rts
                        in (tree', update)
                    else if newUri `isParentOf` uri
                        then let
                            (ch, nonCh) = List.partition 
                                (\(RsyncTree (RsyncPublicationPoint u) _) -> newUri `isParentOf` u) rts
                            in (RsyncTree r (tree : ch) : nonCh, AddedNew)
                        else let 
                            (merged, update) = r `mergeIntoTrees` rts
                            in (tree : merged, update)
        

-- mergeIntoRoots :: RsyncPublicationPoint -> [RsyncRepository] -> [RsyncRepository]
-- mergeIntoRoots r roots = mergeIntoTrees r $ map (\(RsyncRepository t _) -> t) roots      

data RsyncParent = ParentURI !URI | Root !RepositoryStatus
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype RsyncMap = RsyncMap (Map URI RsyncParent)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise


findRepositoryStatus :: URI -> RsyncMap -> Maybe (URI, RepositoryStatus)
findRepositoryStatus u (RsyncMap m) = go u
    where
        go u' =
            Map.lookup u' m >>= \case            
                ParentURI parentUri -> go parentUri
                Root status   -> pure (u, status)


-- | Update repository status and return the old one 
-- | if the uri is pointing to the rsync root.
updateRepositoryStatus :: URI -> RsyncMap -> RepositoryStatus -> (RsyncMap, Maybe RepositoryStatus)
updateRepositoryStatus u rm@(RsyncMap m) status = 
    case Map.lookup u m of
        Nothing            -> (rm, Nothing)
        Just (ParentURI _) -> (rm, Nothing)
        Just (Root s)      -> (RsyncMap $ Map.insert u (Root status) m, Just s)    


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


createRsyncForest :: [RsyncPublicationPoint] -> Forest
createRsyncForest = Forest . List.foldr (\r f -> fst $ r `mergeIntoTrees` f) []

createRsyncMap :: [RsyncPublicationPoint] -> RsyncMap
createRsyncMap = List.foldr (\(RsyncPublicationPoint u) rm -> fst $ u `merge` rm) (RsyncMap Map.empty)


emptyRepositories :: PublicationPoints
emptyRepositories = PublicationPoints Map.empty $ RsyncMap Map.empty 

data RepositoriesUpdated a = AddedNew | AlreadyThere a
    deriving (Show, Eq, Ord)

updateRepositories :: PublicationPoints -> CerObject -> 
                    Either ValidationError (PublicationPoints, RepositoryStatus)
updateRepositories repositories@PublicationPoints {..} c  = do
    (_, publicationPoint) <- publicationPointsFromCert cert
    pure $ updatePublicationPoints repositories publicationPoint
    where
        cert = cwsX509certificate $ getCertWithSignature c


-- | Update repository and return the status of the repository 
-- | as it was before the update.
updatePublicationPoints :: PublicationPoints -> PublicationPoint -> (PublicationPoints, RepositoryStatus)
updatePublicationPoints PublicationPoints {..} repository  = do
    case repository of  
        RrdpPP r@(RrdpRepository { uri = u }) -> 
            case Map.lookup u rrdps of
                Nothing -> (PublicationPoints rrdps' rsyncs, New)
                    where 
                        rrdps' = Map.insert u r rrdps
                Just existing@(RrdpRepository { status = s }) -> (PublicationPoints rrdps' rsyncs, s)
                    where 
                        rrdps' = if r == existing -- most often case
                                then rrdps 
                                else Map.insert u existing rrdps

        RsyncPP (RsyncPublicationPoint u) ->              
            (PublicationPoints rrdps rsyncs', status) 
            where
                (rsyncs', status) = u `merge` rsyncs    


findRepository :: URI -> RsyncTree -> Maybe RsyncPublicationPoint
findRepository childUri (RsyncTree r@(RsyncPublicationPoint parentUri) children)
    | parentUri == childUri           = Just r
    | parentUri `isParentOf` childUri = join $ List.find isJust $ map (findRepository childUri) children
    | otherwise                       = Nothing


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
-- changeSet :: PublicationPoints -> PublicationPoints -> [Change Repository]
-- changeSet (PublicationPoints rrdpOld rsyncOld) (PublicationPoints rrdpNew rsyncNew) = 
--     putNewRrdps <> removeOldRrdps <> putNewRsyncs <> removeOldRsyncs
--     where
--         rom = Map.elems rrdpOld
--         rrdpOldSet = Set.fromList rom
--         rrdpNewSet = Set.fromList $ Map.elems rrdpNew
--         putNewRrdps = map (Put . RrdpR) $ filter (not . (`Set.member` rrdpOldSet)) $ Map.elems rrdpNew
--         removeOldRrdps = map (Remove . RrdpR) $ filter (not . (`Set.member` rrdpNewSet)) rom

--         rsyncOldSet = Set.fromList rsyncOld
--         rsyncNewSet = Set.fromList rsyncNew
--         putNewRsyncs = map (Put . RsyncR . RsyncRepository) $ filter (not . (`Set.member` rsyncOldSet)) rsyncNew
--         removeOldRsyncs = map (Remove . RsyncR . RsyncRepository) $ filter (not . (`Set.member` rsyncNewSet)) rsyncOld


-- reposFromList :: [Repository] -> PublicationPoints
-- reposFromList rs = PublicationPoints rrdps rsync
--     where
--         rrdps = Map.fromList [ (uri, r) | RrdpR r@(RrdpRepository {..}) <- rs ]
--         rsync = [ r | RsyncR r <- rs ]