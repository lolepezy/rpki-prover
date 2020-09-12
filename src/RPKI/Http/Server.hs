{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module RPKI.Http.Server where

import           Control.Monad.IO.Class
import           Servant
import           FileEmbedLzma
import           Servant.Server.StaticFiles

import qualified Data.List.NonEmpty      as NonEmpty

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
                    <$> allVRPs tx vrpStore lastVersion            

        getVResults = 
            getForTheLastVersion $ \tx lastVersion ->             
                map toVR <$> allVResults tx resultStore lastVersion  

        toVR VResult { path = VContext p, .. } = 
            ValidationResult problem (NonEmpty.toList p)

        getForTheLastVersion :: (Tx s 'RO -> WorldVersion -> IO [a]) -> IO [a]
        getForTheLastVersion f = 
            roTx versionStore $ \tx -> do 
                versions' <- allVersions tx versionStore
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
