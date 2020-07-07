{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.Http.Server where

import           Control.Monad.IO.Class

import           Data.List               (maximumBy)
import           Data.Proxy

import           Data.Aeson
import           Servant
import           Servant.API
import           Servant.API.Generic
import           Servant.CSV.Cassava
import           Servant.Server.Generic

import qualified Data.List.NonEmpty      as NonEmpty

import qualified Servant.Types.SourceT   as S

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Http.Api
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Database
import           RPKI.Version

validatorServer :: forall s . Storage s => AppContext s -> Server API
validatorServer appContext@AppContext { database = DB {..}} = 
    liftIO getVRPs
    :<|> liftIO getVRPs
    :<|> liftIO getVResults
    where
        getVRPs = 
            getForTheLastVersion $ \tx lastVersion -> 
                map (\(Roa a p len) -> VRP a p len) 
                    <$> allVRPs tx vrpStore lastVersion            

        getVResults = 
            getForTheLastVersion $ \tx lastVersion ->             
                map toVR <$> allVResults tx resultStore lastVersion  

        toVR VResult { path = VContext p, .. } = 
            ValidationResult problem (NonEmpty.toList p)

        getForTheLastVersion :: (Tx s 'RO -> WorldVersion -> IO [a]) -> IO [a]
        getForTheLastVersion f = 
            roTx versionStore $ \tx -> do 
                versions <- allVersions tx versionStore
                case versions of
                    [] -> pure []
                    vs -> f tx $ maximum [ v | (v, FinishedVersion) <- vs ]                        


httpApi :: Storage s => AppContext s -> Application
httpApi = serve api . validatorServer