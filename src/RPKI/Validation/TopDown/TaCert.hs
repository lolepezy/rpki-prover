{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}

-- | The TA certificate: downloading it, choosing between the downloaded and
-- the cached one, storing it, and reading it for validation.
module RPKI.Validation.TopDown.TaCert where

import           Effectful.Timeout (Timeout)
import           Effectful
import           Control.Concurrent.STM
import           Effectful.Error.Static           (catchError)

import           Control.Lens hiding (children)

import           Data.Generics.Product.Typed

import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch.Fetch
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Repository

import           RPKI.Store.Database    (rwTxT)
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Validation.ObjectValidation


data WhichTA = FetchedTA RpkiURL ParsedRpkiObject | CachedTA

-- | Refresh the TA certificate for the given TAL. Returns whether the
-- | certificate actually changed (a fresh download that differs from the
-- | cached copy, or there was no cached copy at all) -- callers use that to
-- | decide whether the TA needs to be revalidated.
refreshTaCertificate :: AppContext s
                        -> TAL
                        -> WorldVersion
                        -> IO (Either AppError Bool)
refreshTaCertificate appContext@AppContext {..} tal worldVersion = do
    (r, vs) <- runValidatorIO (newScopes' TAFocus (unTaName taName)) $
                    vFocusOn LocationFocus (getURL $ getTaCertURL tal) $ do
                        db <- liftIO $ readTVarIO database
                        storedTa <- DB.roAppTxEx db DB.storageError $ \tx -> DB.getTA tx taName
                        fetchValidateAndStoreTaCert appContext tal worldVersion storedTa

    -- The issues are stored no matter how it went: they are the only way for the
    -- top-down validation to find out what happened here.
    rwTxT database $ \tx -> DB.saveTaValidations tx taName (vs ^. typed)
    pure r
  where
    taName = getTaName tal

-- | Get the TA certificate to start the top-down validation from.
-- | This function doesn't throw exceptions.
taCertificateFromCache :: ValidatorIO es => AppContext s
                        -> TAL
                        -> Eff es (Located WellStructuredCaCert, PublicationPointAccess)
taCertificateFromCache AppContext {..} tal = do
    db <- liftIO $ readTVarIO database
    ta <- DB.roAppTxEx db DB.storageError $ \tx -> DB.getTA tx taName
    case ta of
        Nothing       -> taCertProblem "there's no TA certificate in the cache yet"
        Just storedTa -> do
            (taCert, taValidations) <-
                DB.roAppTxEx db DB.storageError $ \tx ->
                    (,) <$> DB.getTaCertByKey tx (storedTa ^. #taCertKey)
                        <*> DB.getTaValidations tx taName

            embedState $ mempty & typed .~ taValidations

            case taCert of
                -- The object is gone from the cache, the next run of the TA
                -- certificate job will download and store it again.
                Nothing   -> taCertProblem "TA certificate is not in the object cache"
                Just cert -> do
                    let locations = talCertLocations tal <> toLocations (storedTa ^. #actualUrl)
                    pure (locatedTaCert locations cert, storedTa ^. #initialRepositories)
  where
    taName = getTaName tal
    taCertProblem :: Validator es => Text -> Eff es a
    taCertProblem message = appError $ UnspecifiedE (unTaName taName) message

-- | Download the TA certificate using the locations from the TAL, validate it
-- | and store it together with the initial publication points.
-- |
-- | If the download fails, fall back to the cached copy, if there is one.
-- |
-- | This function doesn't throw exceptions.
-- | Download and validate the TA certificate, then store it (or keep the
-- | cached one, if that's what validation prefers). Returns whether the
-- | certificate on file after this call is different from the one that was
-- | cached before it: `True` for a first-ever download or a genuine change,
-- | `False` when the refresh reconfirmed the cached certificate (the common
-- | case, since these RRDP/rsync objects change rarely and are re-fetched
-- | on every refresh) or fell back to it after a download failure.
fetchValidateAndStoreTaCert :: (ValidatorIO es, Timeout :> es) => AppContext s
                        -> TAL
                        -> WorldVersion
                        -> Maybe StorableTA
                        -> Eff es Bool
fetchValidateAndStoreTaCert appContext@AppContext {..} tal worldVersion = go
  where
    go storableTa = do
        db <- liftIO $ readTVarIO database
        cachedTaCertM <- case storableTa of
            Nothing -> pure Nothing
            Just StorableTA { taCertKey } ->
                DB.roAppTxEx db DB.storageError $ \tx ->
                    DB.getTaCertByKey tx taCertKey

        z <- (do 
                (u, ro) <- fetchTACertificate appContext (newFetchConfig config) tal
                pure $ FetchedTA u ro)
            `catchError`
                (\_cs -> tryToFallbackToCachedCopy)

        case z of     
            FetchedTA actualUrl object -> do                                 
                fetchedCert <- validateTACert tal actualUrl object

                (certToUse, certToStore, changed) <- case cachedTaCertM of
                    Nothing  -> pure (fetchedCert, fetchedCert, True)
                    Just cachedTaCert ->
                        (do
                            cert <- chooseTaCert fetchedCert cachedTaCert
                            pure $ if cert == cachedTaCert
                                then (cachedTaCert, cachedTaCert, False)
                                else (fetchedCert, fetchedCert, True))
                        `catchError`
                            (\_cs (e :: AppError) -> do
                                logError logger [i|Fetched TA certificate is invalid with error #{e}, will use cached copy.|]
                                pure (cachedTaCert, cachedTaCert, False))

                case publicationPointsFromTAL tal certToUse of
                    Left e         -> appError $ ValidationE e
                    Right ppAccess -> do
                        DB.rwAppTxEx db DB.storageError $ \tx -> do
                            taCertKey <- DB.saveObject tx (WellStructuredRO (CerRO certToStore)) worldVersion
                            DB.linkObjectToUrl tx actualUrl taCertKey worldVersion
                            DB.saveTA tx (StorableTA tal taCertKey ppAccess actualUrl)
                        pure changed

            -- Nothing was downloaded, the cached copy stays as it is
            CachedTA ->
                case cachedTaCertM of
                    Nothing -> appError $ UnspecifiedE (unTaName $ getTaName tal) "Cached TA cert not found in objects store"
                    Just _  -> pure False

      where
        tryToFallbackToCachedCopy e =
            case storableTa of
                Nothing -> do 
                    logError logger $
                        [i|Could not download TA certiicate for #{getTaName tal}, error: #{e}|] <>
                        [i| and there is no cached copy of it.|]
                    appError e

                Just _ -> do  
                    logError logger $ 
                        [i|Could not download TA certiicate for #{getTaName tal}, error: #{e}|] <> 
                        [i| will use cached copy.|]                                        

                    pure CachedTA

locatedTaCert :: Locations -> WellStructuredCaCert -> Located WellStructuredCaCert
locatedTaCert locations cert = Located (Just locations) cert
