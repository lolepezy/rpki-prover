{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Repository where

import           Control.Monad.IO.Class

import           Control.Monad

import           Codec.Serialise
import           GHC.Generics

import qualified Data.Map.Strict          as Map

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Base.Storage


data RepositoryStore s = RepositoryStore {
    rrdpS  :: SMap "rrdp-repositories" s URI RrdpRepository,
    rsyncS :: SMap "rsync-repositories" s URI RsyncParent,
    perTA  :: SMultiMap "repositories-per-ta" s TaName (URI, RepositoryFetchType)
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _) = storage s


putRepositories :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> PublicationPoints -> TaName -> m ()
putRepositories tx RepositoryStore {..} 
                PublicationPoints { 
                    rsyncs = RsyncMap rsyncs',
                    rrdps = RrdpMap rrdps' } 
                taName' = liftIO $ do    
    forM_ (Map.toList rsyncs') $ \(u, p) -> do
            M.put tx rsyncS u p    
            MM.put tx perTA taName' (u, Rsync)
    forM_ (Map.toList rrdps') $ \(u, p) -> do
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
        -- Do the Remove first and only then Put
        let (rrdpPuts, rrdpRemoves) = separate rrdpChanges        

        forM_ rrdpRemoves $ \RrdpRepository{..} -> do 
                M.delete tx rrdpS uri 
                MM.delete tx perTA taName' (uri, RRDP)
        forM_ rrdpPuts $ \r@RrdpRepository{..} -> do 
                M.put tx rrdpS uri r
                MM.put tx perTA taName' (uri, RRDP)        

        let (rsyncPuts, rsyncRemoves) = separate rsyncChanges

        forM_ rsyncRemoves $ \(uri', _) -> do
                M.delete tx rsyncS uri'
                MM.delete tx perTA taName' (uri', Rsync)                            
        forM_ rsyncPuts $ \(uri', p) -> do
                M.put tx rsyncS uri' p
                MM.put tx perTA taName' (uri', Rsync)
    where
        separate = foldr f ([], [])
            where 
                f (Put r) (ps, rs) = (r : ps, rs) 
                f (Remove r) (ps, rs) = (ps, r : rs) 


getTaPublicationPoints :: (MonadIO m, Storage s) => 
                        Tx s mode -> RepositoryStore s -> TaName -> m PublicationPoints
getTaPublicationPoints tx s taName' = liftIO $ do
        (rrdpList, rsyncList) <- MM.foldS tx (perTA s) taName' mergeAllRepos ([], [])
        pure $ PublicationPoints 
            (RrdpMap $ Map.fromList rrdpList) 
            (RsyncMap $ Map.fromList rsyncList)
    where
        mergeAllRepos result@(rrdps, rsyncs) _ index = 
            case index of 
                (uri', RRDP) -> 
                    M.get tx (rrdpS s) uri' >>= \case
                        Just r  -> pure ((uri', r) : rrdps, rsyncs)
                        Nothing -> pure result                
                (uri', Rsync) -> 
                    M.get tx (rsyncS s) uri' >>= \case
                        Just r  -> pure (rrdps, (uri', r) : rsyncs)
                        Nothing -> pure result
                