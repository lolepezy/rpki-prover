{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Repository where

import           Control.Monad.IO.Class

import           Control.Monad

import           Codec.Serialise
import           GHC.Generics

import qualified Data.List                as List
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Repository
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Base.Storage

import           RPKI.Store.Data



data RepositoryType = Rsync | RRDP
    deriving (Show, Eq, Ord, Generic, Serialise)


data RepositoryStore s = RepositoryStore {
    rrdpS  :: SMap "rrdp-repositories" s URI RrdpRepository,
    rsyncS :: SMap "rsync-repositories" s URI RsyncTree,
    perTA  :: SMultiMap "repositoriesPerTA" s TaName (URI, RepositoryType)
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _) = storage s


putRepository :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> Repository -> TaName -> m ()
putRepository tx RepositoryStore {..} r taName' = liftIO $ do 
    let repoUri = repositoryURI r
    case r of 
        RrdpR rrdp -> do
            M.put tx rrdpS repoUri rrdp
            MM.put tx perTA taName' (repoUri, RRDP)
        RsyncR rsync -> do 
            M.put tx rsyncS repoUri rsync    
            MM.put tx perTA taName' (repoUri, Rsync)
    
    
applyChanges :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> [Change Repository] -> TaName -> m ()
applyChanges tx s changes taName' = 
    forM_ changes $ \case 
        Put r    -> putRepository tx s r taName'
        Remove r -> removeRepositoryRoot tx s r taName'


removeRepositoryRoot :: (MonadIO m, Storage s) => 
                    Tx s 'RW -> RepositoryStore s -> Repository -> TaName -> m ()
removeRepositoryRoot tx RepositoryStore {..} r taName' = liftIO $ do
    let repoUri = repositoryURI r
    case r of 
        RrdpR _ -> do
            M.delete tx rrdpS repoUri 
            MM.delete tx perTA taName' (repoUri, RRDP)
        RsyncR _ -> do 
            M.delete tx rsyncS repoUri
            MM.delete tx perTA taName' (repoUri, Rsync)
            

getAllRepositoriesForTA :: (MonadIO m, Storage s) => 
                        Tx s mode -> RepositoryStore s -> TaName -> m [Repository]
getAllRepositoriesForTA tx s taName' = liftIO $ MM.fold tx (perTA s) taName' f []
    where
        f ros _ index = 
            case index of 
                (uri', Rsync) -> 
                    M.get tx (rsyncS s) uri' >>= \case
                        Just rs -> pure $! (RsyncR rs) : ros
                        Nothing -> pure ros
                (uri', RRDP) -> 
                    M.get tx (rrdpS s) uri' >>= \case
                        Just rs -> pure $! (RrdpR rs) : ros
                        Nothing -> pure ros


getRsyncsForTA :: (MonadIO m, Storage s) => 
                Tx s mode -> RepositoryStore s -> TaName -> m [RsyncTree]
getRsyncsForTA tx s taName' = liftIO $ MM.fold tx (perTA s) taName' f []
    where
        f ros _ index = 
            case index of 
                (uri', Rsync) -> 
                    M.get tx (rsyncS s) uri' >>= \case
                        Just rs -> pure $! rs : ros
                        Nothing -> pure ros
                _ -> pure ros                    

getRrdpsForTA :: (MonadIO m, Storage s) => 
                Tx s mode -> RepositoryStore s -> TaName -> m [RrdpRepository]
getRrdpsForTA tx s taName' = liftIO $ MM.fold tx (perTA s) taName' f []
    where
        f ros _ index = 
            case index of 
                (uri', RRDP) -> 
                    M.get tx (rrdpS s) uri' >>= \case
                        Just rs -> pure $! rs : ros
                        Nothing -> pure ros
                _ -> pure ros                    


getRepositoriesForTA :: (MonadIO m, Storage s) => 
                        Tx s mode -> RepositoryStore s -> TaName -> m Repositories
getRepositoriesForTA tx s taName' = do 
    allRepos <- getAllRepositoriesForTA tx s taName'        
    let (rrdps, rsync) = foldr mergeRepos (Map.empty, []) allRepos
    pure $ Repositories rrdps (List.sort rsync)
    where
        mergeRepos (RrdpR rrdp@(RrdpRepository{..})) (rrdpMap, rsyncList) = 
            let !rrdpMap' = Map.insert uri rrdp rrdpMap
                in (rrdpMap', rsyncList)
        mergeRepos (RsyncR rsync) (rrdpMap, rsyncList) = 
            let !rsyncList' = rsync : rsyncList
                in (rrdpMap, rsyncList')  