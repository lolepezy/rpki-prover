{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Repository where

import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate
import qualified Data.Text                        as T

import           Data.Tuple.Ops

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                        (fmtEx)
import           RPKI.Validation.ObjectValidation 


newtype Repositories = Repositories (Map URI Repository)

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

isRsyncURI, isRrdpURI :: URI -> Bool
isRsyncURI (URI u) = "rsync://" `T.isPrefixOf` u
isRrdpURI (URI u) = "http://" `T.isPrefixOf` u || "https://" `T.isPrefixOf` u                        

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