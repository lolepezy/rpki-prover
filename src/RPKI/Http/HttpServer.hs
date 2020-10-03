{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module RPKI.Http.HttpServer where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           FileEmbedLzma
import           Servant

import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Http.Api
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database


validatorServer :: forall s . Storage s => AppContext s -> Server API
validatorServer AppContext {..} = 
    liftIO getVRPs
    :<|> liftIO getVRPs
    :<|> liftIO getVResults
    :<|> getStats
    :<|> getRpkiObject
    where
        getVRPs = 
            roTx versionStore $ \tx ->
                getLastCompletedVersion database tx >>= \case 
                    Nothing          -> pure []            
                    Just lastVersion -> do
                        vrps <- getVrps tx database lastVersion
                        pure $ map (\(Vrp a p len) -> VrpDto a p len) $ Set.toList vrps                    

        getVResults = 
            roTx versionStore $ \tx -> 
                let txValidations = runMaybeT $ do
                        lastVersion <- MaybeT $ getLastCompletedVersion database tx
                        validations <- MaybeT $ validationsForVersion tx validationsStore lastVersion
                        pure $ map toVR $ validationsToList validations
                in fromMaybe [] <$> txValidations
            
        toVR (VContext path, problems) = 
            ValidationResult (Set.toList problems) (NonEmpty.toList path)    

        getStats = stats database

        getRpkiObject hash = 
            liftIO $ roTx objectStore $ \tx -> 
                (RObject <$>) <$> getByHash tx objectStore hash            

        DB {..} = database


embeddedUI :: Server Raw
embeddedUI = serveDirectoryEmbedded $(embedDir "ui")

httpApi :: Storage s => AppContext s -> Application
httpApi appContext = serve 
                        (Proxy @(API :<|> Raw))
                        (validatorServer appContext :<|> embeddedUI)
