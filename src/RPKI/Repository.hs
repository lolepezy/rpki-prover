{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Repository where

import Data.X509 (Certificate)

import           Control.Concurrent.STM

import           GHC.Generics

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Parse.Parse
import           RPKI.TAL


newtype Repositories = Repositories (Map URI Repository)
    deriving stock (Show, Eq, Ord, Generic)
    

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