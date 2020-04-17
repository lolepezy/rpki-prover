{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module RPKI.Repository where

import           Control.Monad
import           Control.Concurrent.STM
import           Codec.Serialise
import           GHC.Generics

import Data.X509 (Certificate)

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import qualified Data.Text                as Text
import qualified Data.List                as List
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Parse.Parse
import           RPKI.TAL


newtype Repositories = Repositories (Map URI Repository)
    deriving stock (Show, Eq, Ord, Generic)    

data RsyncTree = RsyncTree RsyncRepository [RsyncTree]
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Forest = Forest [RsyncTree]
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

-- | Merge rsync repositories into the hierarchy based on their URIs
-- e.g.
--   rsync://host/foo/
--     rsync://host/foo/bar/
--       rsync://host/foo/bar/quux
--     rsync://host/foo/baz/
--       rsync://host/foo/baz/nup
--       rsync://host/foo/baz/bop
--
mergeInto :: RsyncRepository -> [RsyncTree] -> [RsyncTree]
mergeInto r [] = [RsyncTree r []]
mergeInto 
    r@(RsyncRepository newUri) 
    trees@(tree@(RsyncTree maybeParent@(RsyncRepository uri) children) : rts) = List.sort $ 
        if uri == newUri 
            then trees 
            else if uri `isParentOf` newUri
                then RsyncTree maybeParent (r `mergeInto` children) : rts
                else if newUri `isParentOf` uri
                    then let
                        (ch, nonCh) = List.partition 
                            (\(RsyncTree (RsyncRepository u) _) -> newUri `isParentOf` u) rts
                        in RsyncTree r (tree : ch) : nonCh
                    else tree : r `mergeInto` rts 

createRsyncForest :: [RsyncRepository] -> Forest
createRsyncForest = Forest . List.foldl (flip mergeInto) []


emptyRepositories :: Repositories
emptyRepositories = Repositories Map.empty

updateRepositories :: Repositories -> RpkiObject -> Either ValidationError Repositories
updateRepositories (Repositories m) (CerRO c)  = do
    (uri, repository) <- repositoryFromCert cert
    pure $ Repositories $ Map.insert uri repository m
    where
        cert = cwsX509certificate $ getCertWithSignature c

updateRepositories rs _ = pure rs


-- | Create repository based on URIs in TAL and in TA certificate,
-- | use some reasonable heuristics, but don't try to be very smart.
-- | URI of the repository is supposed to be a "real" one, i.e. where
-- | repository can actually be downloaded from.
createRepositoryFromTAL :: TAL -> CerObject -> Either ValidationError Repository
createRepositoryFromTAL tal (cwsX509certificate . getCertWithSignature -> cert) = 
    case tal of 
        PropertiesTAL {..} -> case prefetchUris of
                                []      -> snd <$> repositoryFromCert cert
                                uri : _ -> fromURI uri
        RFC_TAL {..}       -> snd <$> fromCert
    where        
        fromURI u = 
            if isRsyncURI u 
                then Right $ RsyncRepo $ RsyncRepository u
                else if isRrdpURI u
                    then Right $ RrdpRepo $ RrdpRepository u Nothing
                    else Left $ UnknownUriType u

        fromCert = repositoryFromCert cert

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
                    case [ r | r@(RrdpRepo _) <- uniquePrefetchRepos ] of
                        []    -> uniquePrefetchRepos
                        rrdps -> rrdps        

            pure $ repository :| prefetchReposToUse 

        RFC_TAL {..} -> 
            (\(_, r) -> r :| []) <$> repositoryFromCert cert            
    where        
        fromURI u = 
            if isRsyncURI u 
                then Right (u, RsyncRepo $ RsyncRepository u)
                else if isRrdpURI u
                    then Right (u, RrdpRepo $ RrdpRepository u Nothing)
                    else Left $ UnknownUriType u
        

isRsyncURI, isRrdpURI :: URI -> Bool
isRsyncURI (URI u) = "rsync://" `Text.isPrefixOf` u
isRrdpURI (URI u) = "http://" `Text.isPrefixOf` u || "https://" `Text.isPrefixOf` u                        

isParentOf :: URI -> URI -> Bool
isParentOf (URI maybeParent) (URI maybeChild) = maybeParent `Text.isPrefixOf` maybeChild

repositoryFromCert :: Certificate -> Either ValidationError (URI, Repository)
repositoryFromCert cert = 
    case getRrdpNotifyUri cert of
        Just rrdpNotifyUri | isRrdpURI rrdpNotifyUri -> 
                                Right (rrdpNotifyUri, RrdpRepo $ RrdpRepository rrdpNotifyUri Nothing)
                            | otherwise -> Left $ UnknownUriType rrdpNotifyUri
        Nothing -> 
            case getRepositoryUri cert of 
                Just repositoryUri | isRsyncURI repositoryUri -> 
                                Right (repositoryUri, RsyncRepo $ RsyncRepository repositoryUri)
                                    | otherwise -> Left $ UnknownUriType repositoryUri
                Nothing -> Left CertificateDoesn'tHaveSIA 


-- Utilities
updateIfValidObject :: TVar a -> 
                    (a -> RpkiObject -> Either ValidationError a) -> 
                    RpkiObject -> 
                    STM (Either ValidationError RpkiObject)
updateIfValidObject accumulator handleObject ro = do 
    a <- readTVar accumulator
    case handleObject a ro of
        Left e   -> pure $ Left e
        Right a' -> do 
            writeTVar accumulator a' 
            pure $ Right ro