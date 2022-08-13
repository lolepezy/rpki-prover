{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Validation.BottomUp where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)

import           Data.Either                      (fromRight, partitionEithers)
import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Monoid.Generic
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           UnliftIO.Async

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util (fmtEx, fmtLocations)
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.Common
import           RPKI.AppState

{- 
    Given an object
     - find a path up to a TA certificate
     - validate the chain and the given object     
-}
validateBottomUp :: Storage s => 
                AppContext s 
                -> RpkiObject
                -> Now
                -> ValidatorT IO ()
validateBottomUp 
    appContext@AppContext{..}
    object 
    now = do 
    db@DB {..} <- liftIO $ readTVarIO database    
    case getAKI object of 
        Nothing  -> appError $ ValidationE NoAKI
        Just (AKI ki) -> do 
            parentCert <- roAppTx db $ \tx -> getBySKI tx db (SKI ki)
            case parentCert of 
                Nothing -> appError $ ValidationE ParentCertificateNotFound
                Just pc  -> do                    
                    certPath <- reverse <$> findPathToRoot db pc
                    validateTopDownAlongPath db certPath             
  where
    {- Given a chain of certificatees from a TA to the object, 
       proceed with top-down validation along this chain only.
    -}
    validateTopDownAlongPath db certPath = do
        -- TODO Make it NonEmpty?
        let taCert = head certPath
        let u = pickLocation $ getLocations taCert
        vHoist $ validateTaCertAKI taCert u
        let verifiedResources = createVerifiedResources $ payload taCert
        go verifiedResources certPath
      where        
        go _ [] = pure ()

        go verifiedResources [bottomCert] = do
            (mft, crl) <- validateManifest db bottomCert
            validateOnMft mft object            
            validateObjectItself bottomCert mft crl verifiedResources

        go verifiedResources (cert : certs) = do
            (mft, crl) <- validateManifest db cert            
            let childCert = head certs
            validateOnMft mft childCert            
            Validated validCert    <- vHoist $ validateResourceCert now childCert (cert ^. #payload) crl
            childVerifiedResources <- vHoist $ validateResources (Just verifiedResources) childCert validCert            
            go childVerifiedResources certs
        
        validateOnMft mft o = do 
            let oHash = getHash o
            let mftChildren = mftEntries $ getCMSContent $ cmsPayload mft
            case filter (\(T2 _ h) -> h == oHash) mftChildren of 
                [] -> appError $ ValidationE ObjectNotOnManifest
                _  -> pure ()            


    validateObjectItself bottomCert mft crl verifiedResources =         
        case object of 
            CerRO child -> do 
                Validated validCert <- vHoist $ validateResourceCert now child (bottomCert ^. #payload) crl
                pure ()
            RoaRO roa -> 
                void $ vHoist $ validateRoa now roa bottomCert crl (Just verifiedResources)
            GbrRO gbr -> 
                void $ vHoist $ validateGbr now gbr bottomCert crl (Just verifiedResources)
            RscRO rsc -> 
                void $ vHoist $ validateRsc now rsc bottomCert crl (Just verifiedResources)
            _somethingElse -> do 
                logWarnM logger [i|Unsupported type of object: #{_somethingElse}.|]        


    findPathToRoot db@DB{..} certificate = do                  
        tas <- roAppTx db $ \tx -> getTAs tx taStore         
        let taCerts = Map.fromList $ 
                        map (\(_, StorableTA {..}) ->                             
                        (getSKI taCert, Located (talCertLocations tal) taCert)) tas 
        go taCerts certificate
      where        
        go taCerts cert = 
            case getAKI cert of 
                Nothing       -> appError $ ValidationE NoAKI
                Just (AKI ki) -> do 
                    parentCert <- roAppTx db $ \tx -> getBySKI tx db (SKI ki)
                    case parentCert of 
                        Nothing -> do                         
                            case Map.lookup (SKI ki) taCerts of 
                                Nothing -> appError $ ValidationE ParentCertificateNotFound
                                Just c  -> pure [c]
                        Just pc ->
                            (pc :) <$> go taCerts pc


    validateManifest db@DB{..} certificate = do
        {- This resembles `validateThisCertAndGoDown` from TopDown.hs 
           but the difference is that we don't do any descent down the tree
           and don't track visited object or metrics.
         -}
        let childrenAki   = toAKI $ getSKI certificate
        let certLocations = getLocations certificate                
        maybeMft <- findLatestMft database childrenAki           
        case maybeMft of 
            Nothing -> 
                vError (NoMFT childrenAki certLocations)
                    `catchError`
                    tryLatestValidCachedManifest appContext useManifest maybeMft childrenAki certLocations
                
            Just mft -> 
                useManifest mft childrenAki certLocations
                    `catchError` 
                    tryLatestValidCachedManifest appContext useManifest maybeMft childrenAki certLocations
      where 
        -- TODO Decide what to do with nested scopes (we go bottom up, 
        -- so nesting doesn't work the same way).
        useManifest locatedMft childrenAki certLocations = do 
            let mft = locatedMft ^. #payload
            validateObjectLocations locatedMft
            validateMftLocation locatedMft certificate
            T2 _ crlHash <- case findCrlOnMft mft of 
                        []    -> vError $ NoCRLOnMFT childrenAki certLocations
                        [crl] -> pure crl
                        crls  -> vError $ MoreThanOneCRLOnMFT childrenAki certLocations crls
            
            crlObject <- liftIO $ roTx objectStore $ \tx -> getByHash tx objectStore crlHash
            case crlObject of 
                Nothing -> 
                    vError $ NoCRLExists childrenAki certLocations    

                Just foundCrl@(Located crlLocations (CrlRO crl)) -> do      
                    validateObjectLocations foundCrl           
                    let mftEECert = getEECert $ unCMS $ cmsPayload mft
                    checkCrlLocation foundCrl mftEECert
                    validCrl <- vHoist $ validateCrl now crl certificate
                    pure (mft, validCrl)

                Just _ -> 
                    vError $ CRLHashPointsToAnotherObject crlHash certLocations   
            

