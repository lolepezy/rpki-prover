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
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Lens

import qualified Data.Map.Strict                  as Map

import           Data.String.Interpolate.IsString
import           Data.Tuple.Strict

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.Common

{- 
    Given an object
     - find a path up to a TA certificate
     - validate the chain and the given object     
-}
validateBottomUp :: Storage s => 
                AppContext s 
                -> RpkiObject
                -> Now
                -> ValidatorT IO (Validated RpkiObject)
validateBottomUp 
    appContext@AppContext{..}
    object 
    now = do 
    db <- liftIO $ readTVarIO database    
    case getAKI object of 
        Nothing  -> appError $ ValidationE NoAKI
        Just (AKI ki) -> do 
            parentCert <- roAppTx db $ \tx -> getBySKI tx db (SKI ki)            
            case parentCert of 
                Nothing -> appError $ ValidationE ParentCertificateNotFound
                Just pc  -> do                                        
                    certPath <- reverse . (pc :) <$> findPathToRoot db pc                    
                    vaildManifests <- liftIO $ newTVarIO makeValidManifests
                    validateTopDownAlongPath db certPath vaildManifests         
                    pure $ Validated object    
  where
    {- Given a chain of certificatees from a TA to the object, 
       proceed with top-down validation along this chain only.
    -}
    validateTopDownAlongPath db certPath vaildManifests = do
        -- TODO Make it NonEmpty?
        let taCert = head certPath        
        let locations = getLocations taCert
        vHoist $ inSubObjectVScope (locationsToText $ getLocations taCert) 
               $ validateTaCertAKI taCert (pickLocation locations)
        let verifiedResources = createVerifiedResources $ taCert ^. #payload
        go verifiedResources certPath
      where        
        go _ [] = pure ()

        go verifiedResources [bottomCert] = do            
            inSubObjectVScope (locationsToText $ getLocations bottomCert) $ do
                (mft, crl) <- validateManifest db bottomCert vaildManifests

                -- RSC objects are not supposed to be on a manifest
                case object of
                    RscRO _ -> pure ()
                    _       -> validateOnMft mft object

                validateObjectItself bottomCert crl verifiedResources

        go verifiedResources (cert : certs) = do            
            inSubObjectVScope (locationsToText $ getLocations cert) $ do
                (mft, crl) <- validateManifest db cert vaildManifests            
                let childCert = head certs                
                validateOnMft mft childCert                            
                Validated validCert    <- vHoist $ validateResourceCert @_ @_ @'CACert 
                                                    now childCert (cert ^. #payload) crl
                childVerifiedResources <- vHoist $ validateResources (Just verifiedResources) childCert validCert            
                go childVerifiedResources certs
        
        validateOnMft mft o = do             
            let mftChildren = mftEntries $ getCMSContent $ mft ^. #cmsPayload
            case filter (\(T2 _ h) -> h == getHash o) mftChildren of 
                [] -> appError $ ValidationE ObjectNotOnManifest
                _  -> pure ()            


    validateObjectItself bottomCert crl verifiedResources =         
        inSubObjectVScope "rpki-object" $ 
            case object of 
                CerRO child ->
                    void $ vHoist $ validateResourceCert @_ @_ @'CACert 
                                        now child (bottomCert ^. #payload) crl                
                RoaRO roa -> 
                    void $ vHoist $ validateRoa now roa bottomCert crl (Just verifiedResources)
                GbrRO gbr -> 
                    void $ vHoist $ validateGbr now gbr bottomCert crl (Just verifiedResources)
                AspaRO rsc -> 
                    void $ vHoist $ validateAspa now rsc bottomCert crl (Just verifiedResources)                    
                RscRO rsc -> 
                    void $ vHoist $ validateRsc now rsc bottomCert crl (Just verifiedResources)
                _somethingElse -> do 
                    logWarn logger [i|Unsupported type of object: #{_somethingElse}.|]        


    findPathToRoot db certificate = do                  
        tas <- roAppTx db $ \tx -> getTAs tx db         
        let taCerts = Map.fromList $ 
                        map (\(_, StorableTA {..}) ->                             
                        (getSKI taCert, Located (talCertLocations tal) taCert)) tas 
        go taCerts certificate
      where        
        go taCerts cert = do             
            case getAKI cert of 
                -- it is likely a TA certificate which we also downloaded from the repository
                Nothing  -> do 
                    case filter (\(_, c) -> getHash c == getHash cert) $ Map.toList taCerts of 
                        [] -> appError $ ValidationE NoAKI
                        _  -> pure []
                Just (AKI ki) -> do 
                    parentCert <- roAppTx db $ \tx -> getBySKI tx db (SKI ki)
                    case parentCert of 
                        Nothing -> do                         
                            case Map.lookup (SKI ki) taCerts of 
                                Nothing -> appError $ ValidationE ParentCertificateNotFound
                                Just c  -> pure [c]
                        Just pc ->
                            (pc :) <$> go taCerts pc


    validateManifest db certificate validManifests = do
        {- This resembles `validateThisCertAndGoDown` from TopDown.hs 
           but the difference is that we don't do any descent down the tree
           and don't track visited object or metrics.
         -}
        let childrenAki   = toAKI $ getSKI certificate
        let certLocations = getLocations certificate                
        maybeMft <- liftIO $ roTx db $ \tx -> findLatestMftByAKI tx db childrenAki
        let tryLatestValid = tryLatestValidCachedManifest appContext useManifest 
                                ((^. #object) <$> maybeMft) validManifests childrenAki certLocations
        case maybeMft of 
            Nothing -> 
                vError (NoMFT childrenAki) `catchError` tryLatestValid                
            Just mft -> 
                useManifest mft childrenAki certLocations `catchError` tryLatestValid
      where 
        -- TODO Decide what to do with nested scopes (we go bottom up, 
        -- so nesting doesn't work the same way).
        useManifest keyedMft childrenAki certLocations = do 
            let locatedMft = keyedMft ^. #object
            let mft = locatedMft ^. #payload            
            validateObjectLocations locatedMft
            validateMftLocation locatedMft certificate
            T2 _ crlHash <- case findCrlOnMft mft of 
                        []    -> vError $ NoCRLOnMFT childrenAki
                        [crl] -> pure crl
                        crls  -> vError $ MoreThanOneCRLOnMFT childrenAki crls
            
            crlObject <- liftIO $ roTx db $ \tx -> getByHash tx db crlHash
            case crlObject of 
                Nothing -> 
                    vError $ NoCRLExists childrenAki    

                Just foundCrl@(Located _ (CrlRO crl)) -> do      
                    validateObjectLocations foundCrl           
                    let mftEECert = getEECert $ unCMS $ cmsPayload mft
                    checkCrlLocation foundCrl mftEECert
                    validCrl <- vHoist $ validateCrl now crl certificate
                    pure (mft, validCrl)

                Just _ -> 
                    vError $ CRLHashPointsToAnotherObject crlHash   

