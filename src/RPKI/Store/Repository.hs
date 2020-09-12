{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Repository where

import           Control.Monad.IO.Class

import           Control.Monad

import qualified Data.Map.Strict          as Map

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Base.Storage


data RepositoryStore s = RepositoryStore {
    rrdpS  :: SMap "rrdp-repositories" s RrdpURL RrdpRepository,
    rsyncS :: SMap "rsync-repositories" s RsyncURL RsyncParent,
    lastS  :: SMap "last-fetch-success" s RpkiURL FetchLastSuccess,
    perTA  :: SMultiMap "repositories-per-ta" s TaName RpkiURL
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _ _) = storage s


putRepositories :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> PublicationPoints -> TaName -> m ()
putRepositories tx RepositoryStore {..} 
                PublicationPoints { 
                    rsyncs = RsyncMap rsyncs',
                    rrdps = RrdpMap rrdps', 
                    lastSucceded = LastSuccededMap lastSucceded'} 
                taName' = liftIO $ do    
    forM_ (Map.toList rsyncs') $ \(u, p) -> do
            M.put tx rsyncS u p    
            MM.put tx perTA taName' $ RsyncU u
    forM_ (Map.toList rrdps') $ \(u, p) -> do
            M.put tx rrdpS u p    
            MM.put tx perTA taName' $ RrdpU u
    forM_ (Map.toList lastSucceded') $ \(u, p) -> 
            M.put tx lastS u p                

    
updateRrdpMeta :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> (SessionId, Serial) -> RrdpURL -> m ()
updateRrdpMeta tx RepositoryStore {..} meta url = liftIO $ 
    M.get tx rrdpS url >>= \case    
        Nothing -> pure ()
        Just r  -> M.put tx rrdpS url (r { rrdpMeta = Just meta })    

applyChangeSet :: (MonadIO m, Storage s) => 
                Tx s 'RW -> 
                RepositoryStore s -> 
                ChangeSet -> 
                TaName -> 
                m ()
applyChangeSet tx RepositoryStore {..} (ChangeSet rrdpChanges rsyncChanges lastSucceded) taName' = 
    liftIO $ do
        -- Do the Remove first and only then Put
        let (rrdpPuts, rrdpRemoves) = separate rrdpChanges        

        forM_ rrdpRemoves $ \RrdpRepository{..} -> do 
                M.delete tx rrdpS uri 
                MM.delete tx perTA taName' (RrdpU uri)
        forM_ rrdpPuts $ \r@RrdpRepository{..} -> do 
                M.put tx rrdpS uri r
                MM.put tx perTA taName' (RrdpU uri)        

        let (rsyncPuts, rsyncRemoves) = separate rsyncChanges

        forM_ rsyncRemoves $ \(uri', _) -> do
                M.delete tx rsyncS uri'
                MM.delete tx perTA taName' (RsyncU uri')                            
        forM_ rsyncPuts $ \(uri', p) -> do
                M.put tx rsyncS uri' p
                MM.put tx perTA taName' (RsyncU uri')

        let (lastSPuts, lastSRemoves) = separate lastSucceded
        forM_ lastSRemoves $ \(uri', _) -> 
                M.delete tx lastS uri'                
        forM_ lastSPuts $ \(uri', p) ->
                M.put tx lastS uri' p                
    where
        separate = foldr f ([], [])
            where 
                f (Put r) (ps, rs) = (r : ps, rs) 
                f (Remove r) (ps, rs) = (ps, r : rs) 


getTaPublicationPoints :: (MonadIO m, Storage s) => 
                        Tx s mode -> RepositoryStore s -> TaName -> m PublicationPoints
getTaPublicationPoints tx s taName' = liftIO $ do
        (rrdpList, rsyncList, lastSucceded) <- MM.foldS 
            tx (perTA s) taName' mergeAllRepos ([], [], [])
        pure $ PublicationPoints 
            (RrdpMap $ Map.fromList rrdpList) 
            (RsyncMap $ Map.fromList rsyncList)
            (LastSuccededMap $ Map.fromList lastSucceded)
    where
        mergeAllRepos (rrdps, rsyncs, lastSucc) _ indexUrl = do
            z <- M.get tx (lastS s) indexUrl 
            let lastSucc' = case z of
                    Nothing -> lastSucc
                    Just ls -> (indexUrl, ls) : lastSucc
            
            let result = (rrdps, rsyncs, lastSucc')
            case indexUrl of 
                RrdpU uri' -> 
                    M.get tx (rrdpS s) uri' >>= \case
                        Just r  -> pure ((uri', r) : rrdps, rsyncs, lastSucc')
                        Nothing -> pure result
                RsyncU uri' -> 
                    M.get tx (rsyncS s) uri' >>= \case
                        Just r  -> pure (rrdps, (uri', r) : rsyncs, lastSucc')
                        Nothing -> pure result
                