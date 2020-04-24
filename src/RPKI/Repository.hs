{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
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

data RsyncRepository = RsyncRepository {
    uri    :: !URI,
    status :: !RepositoryStatus
} deriving stock (Show, Eq, Ord, Generic)

data RrdpRepository = RrdpRepository {
    uri      :: !URI,
    rrdpMeta :: !(Maybe (SessionId, Serial)),
    status   :: !RepositoryStatus
} deriving stock (Show, Eq, Ord, Generic)

data Repository = RrdpR !RrdpRepository | 
                  RsyncR !RsyncTree
    deriving (Show, Eq, Ord, Generic, Serialise) 

data RsyncTree = RsyncTree RsyncRepository [RsyncTree]
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Forest = Forest [RsyncTree]
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data Repositories = Repositories {
    rrdps  :: Map URI RrdpRepository,
    rsyncs :: [RsyncTree]
} deriving stock (Show, Eq, Ord, Generic)   

instance Serialise RsyncRepository
instance Serialise RrdpRepository

rsyncR :: URI -> Repository
rsyncR u = RsyncR $ RsyncTree (RsyncRepository u New) []

rrdpR :: URI -> Repository
rrdpR u = RrdpR $ RrdpRepository u Nothing New 

repositoryURI :: Repository -> URI
repositoryURI (RrdpR (RrdpRepository {..})) = uri
repositoryURI (RsyncR (RsyncTree (RsyncRepository {..}) _)) = uri


-- | Merge rsync repositories into the hierarchy based on their URIs
-- e.g.
--   rsync://host/foo/
--     rsync://host/foo/bar/
--       rsync://host/foo/bar/quux
--     rsync://host/foo/baz/
--       rsync://host/foo/baz/nup
--       rsync://host/foo/baz/bop
--
mergeInto :: RsyncRepository -> [RsyncTree] -> ([RsyncTree], RepositoriesUpdated)
mergeInto r [] = ([RsyncTree r []], AddedNew)
mergeInto 
    r@(RsyncRepository newUri _) 
    trees@(tree@(RsyncTree maybeParent@(RsyncRepository uri _) children) : rts) = first List.sort $ 
        if uri == newUri 
            then (trees, AlreadyThere $ RsyncR tree)
            else                 
                if uri `isParentOf` newUri
                    then let 
                        (merged, update) = r `mergeInto` children
                        tree' = RsyncTree maybeParent merged : rts
                        in (tree', update)
                    else if newUri `isParentOf` uri
                        then let
                            (ch, nonCh) = List.partition 
                                (\(RsyncTree (RsyncRepository u _) _) -> newUri `isParentOf` u) rts
                            in (RsyncTree r (tree : ch) : nonCh, AddedNew)
                        else 
                            let 
                                (merged, update) = r `mergeInto` rts
                                in (tree : merged, update)


createRsyncForest :: [RsyncRepository] -> Forest
createRsyncForest = Forest . List.foldr (\r f -> fst $ r `mergeInto` f) []


emptyRepositories :: Repositories
emptyRepositories = Repositories Map.empty []

data RepositoriesUpdated = AddedNew | AlreadyThere Repository
    deriving (Show, Eq, Ord)

updateRepositories :: Repositories -> CerObject -> Either ValidationError (Repositories, RepositoriesUpdated)
updateRepositories repositories@Repositories {..} c  = do
    (uri, repository) <- repositoryFromCert cert
    pure $ case repository of  
        RrdpR rrdpRepository -> 
            case Map.lookup uri rrdps of
                Nothing -> (newR, AddedNew)
                    where 
                        newR = Repositories (Map.insert uri rrdpRepository rrdps) rsyncs
                Just r  -> (newR, AlreadyThere $ RrdpR r)
                    where 
                        newR = if r == rrdpRepository -- most often case
                                then repositories 
                                else Repositories (Map.insert uri rrdpRepository rrdps) rsyncs

        RsyncR (RsyncTree rsyncRepository _) ->              
            (Repositories rrdps rsyncs', update) 
            where
                (rsyncs', update) = rsyncRepository `mergeInto` rsyncs
    where
        cert = cwsX509certificate $ getCertWithSignature c


findRepository :: URI -> RsyncTree -> Maybe RsyncRepository
findRepository childUri (RsyncTree r@(RsyncRepository parentUri _) children)
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
            (certUri, repository) <- repositoryFromCert cert
            uniquePrefetchRepos   <- mapM 
                (\u -> snd <$> fromURI u) $ filter (/= certUri) prefetchUris

            let prefetchReposToUse = 
                    case [ r | r@(RrdpR _) <- uniquePrefetchRepos ] of
                        []    -> uniquePrefetchRepos
                        rrdps -> rrdps

            pure $ repository :| prefetchReposToUse 

        RFC_TAL {..} -> 
            (\(_, r) -> r :| []) <$> repositoryFromCert cert            
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
repositoryFromCert :: Certificate -> Either ValidationError (URI, Repository)
repositoryFromCert cert = 
    case (getRrdpNotifyUri cert, getRepositoryUri cert) of
        (Just rrdpNotifyUri, _) 
            | isRrdpURI rrdpNotifyUri -> Right (rrdpNotifyUri, rrdpR rrdpNotifyUri)
            | otherwise               -> Left $ UnknownUriType rrdpNotifyUri
        (Nothing, Just repositoryUri) 
            | isRsyncURI repositoryUri -> Right (repositoryUri, rsyncR repositoryUri)
            | otherwise                -> Left $ UnknownUriType repositoryUri
        (Nothing, Nothing)             -> Left CertificateDoesn'tHaveSIA 

repositoryFromCertObject :: CerObject -> Either ValidationError (URI, Repository)
repositoryFromCertObject = repositoryFromCert . cwsX509certificate . getCertWithSignature

data Change a = Put a | Remove a 


-- | Derive a change set to apply to the 
changeSet :: Repositories -> Repositories -> [Change Repository]
changeSet (Repositories rrdpOld rsyncOld) (Repositories rrdpNew rsyncNew) = 
    putNewRrdps <> removeOldRrdps <> putNewRsyncs <> removeOldRsyncs
    where
        rom = Map.elems rrdpOld
        rrdpOldSet = Set.fromList rom
        rrdpNewSet = Set.fromList $ Map.elems rrdpNew
        putNewRrdps = map (Put . RrdpR) $ filter (not . (`Set.member` rrdpOldSet)) $ Map.elems rrdpNew
        removeOldRrdps = map (Remove . RrdpR) $ filter (not . (`Set.member` rrdpNewSet)) rom

        rsyncOldSet = Set.fromList rsyncOld
        rsyncNewSet = Set.fromList rsyncNew
        putNewRsyncs = map (Put . RsyncR) $ filter (not . (`Set.member` rsyncOldSet)) rsyncNew
        removeOldRsyncs = map (Remove . RsyncR) $ filter (not . (`Set.member` rsyncNewSet)) rsyncOld


reposFromList :: [Repository] -> Repositories
reposFromList rs = Repositories rrdps (List.sort rsync)
    where
        rrdps = Map.fromList [ (uri, r) | RrdpR r@(RrdpRepository {..}) <- rs ]
        rsync = [ r | RsyncR r <- rs ]