{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module RPKI.Http.Server where

import           Control.Monad.IO.Class
import           FileEmbedLzma
import           Servant
import           Servant.Server.StaticFiles

import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Set            as Set

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Http.Api
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Database
import           RPKI.Version



validatorServer :: forall s . Storage s => AppContext s -> Server API
validatorServer AppContext {..} = 
    liftIO getVRPs
    :<|> liftIO getVRPs
    :<|> liftIO getVResults
    :<|> getStats
    :<|> getRpkiObject
    where
        getVRPs = 
            getForTheLastVersion $ \tx lastVersion -> 
                map (\(Vrp a p len) -> VrpDto a p len) 
                    <$> getVrps tx database lastVersion            

        getVResults = 
            getForTheLastVersion $ \tx lastVersion ->
                validationsForVersion tx validationsStore lastVersion >>= \case 
                    Nothing          -> pure []
                    Just validations -> pure $ map toVR $ validationsToList validations

        toVR (VContext path, problems) = 
            ValidationResult (Set.toList problems) (NonEmpty.toList path)

        getForTheLastVersion :: (Tx s 'RO -> WorldVersion -> IO [a]) -> IO [a]
        getForTheLastVersion f = 
            roTx versionStore $ \tx -> do 
                versions' <- allVersions tx database
                case versions' of
                    [] -> pure []
                    vs -> f tx $ maximum [ v | (v, FinishedVersion) <- vs ]      

        getStats = stats database

        getRpkiObject hash = 
            liftIO $ roTx objectStore $ \tx -> 
                (RObject <$>) <$> getByHash tx objectStore hash            

        DB {..} = database


embeddedUI :: Server Raw
embeddedUI = serveDirectoryEmbedded $(embedDir "ui")

httpApi :: Storage s => AppContext s -> Application
httpApi appContext = serve 
                        (Proxy :: Proxy (API :<|> Raw))
                        (validatorServer appContext :<|> embeddedUI)
