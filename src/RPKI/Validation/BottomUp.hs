{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RPKI.Validation.BottomUp where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Lens

import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as Text

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
                -> ParsedRpkiObject
                -> Now
                -> ValidatorT IO (Validated ParsedRpkiObject, [[Located WellStructuredCaCert]])
validateBottomUp 
    AppContext{..}
    object 
    now = do 
    db <- liftIO $ readTVarIO database    
    case getAKI object of 
        Nothing  -> appError $ ValidationE NoAKI
        Just (AKI ki) -> do 
            z <- DB.roAppTx db $ \tx -> DB.getBySKI tx db (SKI ki)            
            case z of 
                []          -> appError $ ValidationE ParentCertificateNotFound
                parentCerts ->                               
                    fmap (Validated object, ) 
                    $ fmap mconcat 
                    $ forM parentCerts $ \pc -> do 
                        pathsToRoot <- findPathsToRoot db pc
                        -- certPath <- reverse . (pc :) <$> findPathsToRoot db pc
                        forM pathsToRoot $ \path -> do
                            let topDownPath = reverse $ pc : path
                            validateTopDownAlongPath db topDownPath
                            pure topDownPath
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
                Validated validCert <- vHoist $ validateResourceCertV
                                                now
                                                (childCert ^. #payload)
                                                (cert ^. #payload)
                                                crl
                (childVerifiedResources, _) <- vHoist $ validateResourcesCAV
                                                    validationRFC
                                                    (Just verifiedResources)
                                                    validCert
                                                    (cert ^. #payload)
                go childVerifiedResources certs
        
        validateOnMft mft o = do             
            let mftChildren = mftEntries $ mft ^. #content
            case filter (\(MftPair _ h) -> h == getHash o) mftChildren of 
                [] -> appError $ ValidationE ObjectNotOnManifest
                _  -> pure ()            


    validateObjectItself bottomCert crl verifiedResources =
        vFocusOn TextFocus "rpki-object" $ do
            validatedObject <- vHoist $ prevalidateObject object
            case validatedObject of
                CerRO child ->
                    void $ vHoist $ validateResourceCert now child (bottomCert ^. #payload) crl
                MftRO mft ->
                    void $ vHoist $ validateMft validationRFC now mft (bottomCert ^. #payload) crl (Just verifiedResources)
                RoaRO roa ->
                    void $ vHoist $ validateRoa validationRFC now roa (bottomCert ^. #payload) crl (Just verifiedResources)
                SplRO spl ->
                    void $ vHoist $ validateSpl validationRFC now spl (bottomCert ^. #payload) crl (Just verifiedResources)
                GbrRO gbr ->
                    void $ vHoist $ validateGbr validationRFC now gbr (bottomCert ^. #payload) crl (Just verifiedResources)
                RscRO rsc ->
                    void $ vHoist $ validateRsc validationRFC now rsc (bottomCert ^. #payload) crl (Just verifiedResources)
                AspaRO aspa ->
                    void $ vHoist $ validateAspa validationRFC now aspa (bottomCert ^. #payload) crl (Just verifiedResources)
                BgpRO bgp ->
                    void $ vHoist $ validateBgpCert now bgp (bottomCert ^. #payload) crl
                CrlRO childCrl ->
                    void $ vHoist $ validateCrl now childCrl (bottomCert ^. #payload)


    -- Given a certificate, find a chain of certificates leading to a TA, 
    -- the chain is build based on the SKI - AKI relations
    findPathsToRoot db certificate = do                  
        tas <- DB.roAppTx db $ \tx -> DB.getTAs tx db         
        let taCerts = Map.fromList [ 
                        (getSKI taCert, Located (talCertLocations tal) taCert) | 
                        StorableTA {..} <- tas ]
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
                    parentCerts <- DB.roAppTx db $ \tx -> DB.getBySKI tx db (SKI ki)
                    case parentCerts of 
                        [] ->                       
                            case Map.lookup (SKI ki) taCerts of 
                                Nothing -> appError $ ValidationE ParentCertificateNotFound
                                Just c  -> pure [[c]]
                        parents -> 
                            fmap mconcat $ forM parents $ \pc -> 
                                map (pc: ) <$> go taCerts pc


    validateManifest db certificate = do
        {- This resembles `validateThisCertAndGoDown` from TopDown.hs 
           but the difference is that we don't do any descent down the tree
           and don't track visited object or metrics.
         -}
        let childrenAki = toAKI $ getSKI certificate
        maybeMft <- liftIO $ roTx db $ \tx -> do 
            DB.getMftsForAKI tx db childrenAki >>= \case
                [] -> pure Nothing
                (MftMeta {..} : _) -> DB.getMftByKey tx db key
        case maybeMft of 
            Nothing -> 
                vError $ NoMFT childrenAki
            Just keyedMft -> do
                -- TODO Decide what to do with nested scopes (we go bottom up, 
                -- so nesting doesn't work the same way).                            
                let Keyed locatedMft@(Located mftLocation mft) _ = keyedMft
                vFocusOn LocationFocus (getURL $ pickLocation mftLocation) $ do                
                    validateObjectLocations locatedMft
                    validateMftLocation locatedMft certificate
                    MftPair _ crlHash <- 
                            case filter (\(MftPair name _) -> ".crl" `Text.isSuffixOf` name) $ mftEntries $ mft ^. #content of
                                []    -> vError $ NoCRLOnMFT childrenAki
                                [crl] -> pure crl
                                crls  -> vError $ MoreThanOneCRLOnMFT childrenAki crls
                    
                    crlObject <- liftIO $ roTx db $ \tx -> DB.getByHash tx db crlHash
                    case crlObject of 
                        Nothing -> 
                            vError $ NoCRLExists childrenAki crlHash

                        Just foundCrl@(Located crlLocations (WellStructuredRO (CrlRO crl))) -> do
                            vFocusOn LocationFocus (getURL $ pickLocation crlLocations) $ do 
                                validateObjectLocations foundCrl
                                checkCrlLocation foundCrl $ eeCert mft
                                validCrl <- vHoist $ validateCrl now crl (certificate ^. #payload)
                                pure (mft, validCrl)

                        Just _ -> 
                            vError $ CRLHashPointsToAnotherObject crlHash   

