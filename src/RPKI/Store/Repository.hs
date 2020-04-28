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
    rsyncS :: SMap "rsync-repositories" s URI RsyncParent,
    perTA  :: SMultiMap "repositories-per-ta" s TaName (URI, RepositoryType)
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _) = storage s


-- putRepository :: (MonadIO m, Storage s) => 
--                 Tx s 'RW -> RepositoryStore s -> Repository -> TaName -> m ()
-- putRepository tx RepositoryStore {..} r taName' = liftIO $ do 
--     let repoUri = repositoryURI r
--     case r of 
--         RrdpR rrdp -> do
--             M.put tx rrdpS repoUri rrdp
--             MM.put tx perTA taName' (repoUri, RRDP)
--         RsyncR rsync -> do 
--             M.put tx rsyncS repoUri rsync    
--             MM.put tx perTA taName' (repoUri, Rsync)

putRepositories :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> PublicationPoints -> TaName -> m ()
putRepositories tx RepositoryStore {..} PublicationPoints {..} taName' = liftIO $ do
    let RsyncMap rsyncs' = rsyncs
    forM_ (Map.toList rsyncs') $ \(u, p) -> do
            M.put tx rsyncS u p    
            MM.put tx perTA taName' (u, Rsync)
    forM_ (Map.toList rrdps) $ \(u, p) -> do
            M.put tx rrdpS u p    
            MM.put tx perTA taName' (u, RRDP)

    
applyChangeSet :: (MonadIO m, Storage s) => 
                Tx s 'RW -> 
                RepositoryStore s -> 
                ([Change RrdpRepository], [Change (URI, RsyncParent)]) -> 
                TaName -> 
                m ()
applyChangeSet tx RepositoryStore {..} (rrdpChanges, rsyncChanges) taName' = 
    liftIO $ do
        forM_ rrdpChanges $ \case 
            Put r@(RrdpRepository{..})  -> do
                M.put tx rrdpS uri r
                MM.put tx perTA taName' (uri, RRDP)                        
            Remove (RrdpRepository{..}) -> do 
                M.delete tx rrdpS uri 
                MM.delete tx perTA taName' (uri, RRDP)
        forM_ rsyncChanges $ \case 
            Put (uri', p)  -> do
                M.put tx rsyncS uri' p
                MM.put tx perTA taName' (uri', RRDP)                        
            Remove (uri', _) -> do 
                M.delete tx rsyncS uri'
                MM.delete tx perTA taName' (uri', RRDP)        


getRepositoriesForTA :: (MonadIO m, Storage s) => 
                        Tx s mode -> RepositoryStore s -> TaName -> m PublicationPoints
getRepositoriesForTA tx s taName' = liftIO $ do
        (rrdpList, rsyncList) <- MM.fold tx (perTA s) taName' mergeRepos ([], [])
        pure $ PublicationPoints (Map.fromList rrdpList) (RsyncMap $ Map.fromList rsyncList)
    where
        mergeRepos result@(rrdps, rsyncs) _ index = 
            case index of 
                (uri', RRDP) -> 
                    M.get tx (rrdpS s) uri' >>= \case
                        Just r  -> pure $! ((uri', r) : rrdps, rsyncs)
                        Nothing -> pure result                
                (uri', Rsync) -> 
                    M.get tx (rsyncS s) uri' >>= \case
                        Just r  -> pure $! (rrdps, (uri', r) : rsyncs)
                        Nothing -> pure result
                