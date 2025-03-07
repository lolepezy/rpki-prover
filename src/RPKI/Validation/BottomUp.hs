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
import           Control.Lens

import qualified Data.Map.Strict                  as Map

import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database    as DB
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
                -> ValidatorT IO (Validated RpkiObject, [Located CaCerObject])
validateBottomUp 
    AppContext{..}
    object 
    now = do 
    db <- liftIO $ readTVarIO database    
    case getAKI object of 
        Nothing  -> appError $ ValidationE NoAKI
        Just (AKI ki) -> do 
            parentCert <- DB.roAppTx db $ \tx -> DB.getBySKI tx db (SKI ki)            
            case parentCert of 
                Nothing -> appError $ ValidationE ParentCertificateNotFound
                Just pc  -> do                                        
                    certPath <- reverse . (pc :) <$> findPathToRoot db pc                                        
                    validateTopDownAlongPath db certPath 
                    pure (Validated object, certPath)
  where
    validationRFC = config ^. #validationConfig . #validationRFC
    {- Given a chain of certificatees from a TA to the object, 
       proceed with top-down validation along this chain only.
    -}
    validateTopDownAlongPath db certPath = do
        -- TODO Make it NonEmpty?
        let taCert = head certPath        
        let location = pickLocation $ getLocations taCert
        vHoist $ vFocusOn LocationFocus (getURL location) 
               $ validateTaCertAKI taCert location
        let verifiedResources = createVerifiedResources $ taCert ^. #payload        
        go verifiedResources certPath
      where                
        go _ [] = pure ()

        go verifiedResources [bottomCert] = do            
            vFocusOn LocationFocus (getURL $ pickLocation $ getLocations bottomCert) $ do
                (mft, crl) <- validateManifest db bottomCert

                -- RSC objects are not supposed to be on a manifest
                case object of
                    RscRO _ -> pure ()
                    _       -> validateOnMft mft object

                validateObjectItself bottomCert crl verifiedResources

        go verifiedResources (cert : certs) = do            
            vFocusOn LocationFocus (getURL $ pickLocation $ getLocations cert) $ do
                (mft, crl) <- validateManifest db cert
                let childCert = head certs                
                validateOnMft mft childCert                            
                Validated validCert    <- vHoist $ validateResourceCert @_ @_ @'CACert 
                                                    now childCert cert crl
                (childVerifiedResources, _) <- vHoist $ validateResources validationRFC
                                                    (Just verifiedResources) childCert validCert            
                go childVerifiedResources certs
        
        validateOnMft mft o = do             
            let mftChildren = mftEntries $ getCMSContent $ mft ^. #cmsPayload
            case filter (\(MftPair _ h) -> h == getHash o) mftChildren of 
                [] -> appError $ ValidationE ObjectNotOnManifest
                _  -> pure ()            


    validateObjectItself bottomCert crl verifiedResources =         
        vFocusOn TextFocus "rpki-object" $ 
            case object of 
                CerRO child ->
                    void $ vHoist $ validateResourceCert @_ @_ @'CACert 
                                        now child bottomCert crl                
                RoaRO roa -> 
                    void $ vHoist $ validateRoa validationRFC now roa bottomCert crl (Just verifiedResources)
                GbrRO gbr -> 
                    void $ vHoist $ validateGbr validationRFC now gbr bottomCert crl (Just verifiedResources)
                AspaRO rsc -> 
                    void $ vHoist $ validateAspa validationRFC now rsc bottomCert crl (Just verifiedResources)                    
                RscRO rsc -> 
                    void $ vHoist $ validateRsc validationRFC now rsc bottomCert crl (Just verifiedResources)
                _somethingElse -> do 
                    logWarn logger [i|Unsupported type of object: #{_somethingElse}.|]        


    -- Given a certificate, find a chain of certificates leading to a TA, 
    -- the chain is build based on the SKI - AKI relations
    findPathToRoot db certificate = do                  
        tas <- DB.roAppTx db $ \tx -> DB.getTAs tx db         
        let taCerts = Map.fromList [ 
                        (getSKI taCert, Located (talCertLocations tal) taCert) | 
                        (_, StorableTA {..}) <- tas ]
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
                    parentCert <- DB.roAppTx db $ \tx -> DB.getBySKI tx db (SKI ki)
                    case parentCert of 
                        Nothing -> do                         
                            case Map.lookup (SKI ki) taCerts of 
                                Nothing -> appError $ ValidationE ParentCertificateNotFound
                                Just c  -> pure [c]
                        Just pc ->
                            (pc :) <$> go taCerts pc


    validateManifest db certificate = do
        {- This resembles `validateThisCertAndGoDown` from TopDown.hs 
           but the difference is that we don't do any descent down the tree
           and don't track visited object or metrics.
         -}
        let childrenAki = toAKI $ getSKI certificate
        maybeMft <- liftIO $ roTx db $ \tx -> DB.findLatestMftByAKI tx db childrenAki        
        case maybeMft of 
            Nothing -> 
                vError $ NoMFT childrenAki
            Just keyedMft -> do
                -- TODO Decide what to do with nested scopes (we go bottom up, 
                -- so nesting doesn't work the same way).                            
                let locatedMft@(Located mftLocation mft) = keyedMft ^. #object    
                vFocusOn LocationFocus (getURL $ pickLocation mftLocation) $ do                
                    validateObjectLocations locatedMft
                    validateMftLocation locatedMft certificate
                    MftPair _ crlHash <- 
                            case findCrlOnMft mft of 
                                []    -> vError $ NoCRLOnMFT childrenAki
                                [crl] -> pure crl
                                crls  -> vError $ MoreThanOneCRLOnMFT childrenAki crls
                    
                    crlObject <- liftIO $ roTx db $ \tx -> DB.getByHash tx db crlHash
                    case crlObject of 
                        Nothing -> 
                            vError $ NoCRLExists childrenAki crlHash

                        Just foundCrl@(Located crlLocations (CrlRO crl)) -> do      
                            vFocusOn LocationFocus (getURL $ pickLocation crlLocations) $ do 
                                validateObjectLocations foundCrl
                                let mftEECert = getEECert $ unCMS $ cmsPayload mft
                                checkCrlLocation foundCrl mftEECert
                                validCrl <- vHoist $ validateCrl now crl certificate
                                pure (mft, validCrl)

                        Just _ -> 
                            vError $ CRLHashPointsToAnotherObject crlHash   

