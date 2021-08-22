{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Repository where

import           Control.Monad.IO.Class

import           Control.Monad

import qualified Data.Map.Strict          as Map

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Repository
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Base.Storage


data RepositoryStore s = RepositoryStore {
    rrdpS  :: SMap "rrdp-repositories" s RrdpURL RrdpRepository,
    rsyncS :: SMap "rsync-repositories" s RsyncURL RsyncParent,
    lastS  :: SMap "last-fetch-success" s RpkiURL FetchEverSucceeded 
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _) = storage s


putRepositories :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> PublicationPoints -> TaName -> m ()
putRepositories tx RepositoryStore {..} 
                PublicationPoints { 
                    rsyncs = RsyncMap rsyncs',
                    rrdps = RrdpMap rrdps', 
                    lastSucceded = EverSucceededMap lastSucceded'} 
                taName' = liftIO $ do    
    forM_ (Map.toList rsyncs') $ \(u, p) ->
            M.put tx rsyncS u p    
    forM_ (Map.toList rrdps') $ \(u, p) -> 
            M.put tx rrdpS u p    
    forM_ (Map.toList lastSucceded') $ \(u, p) -> 
            M.put tx lastS u p                

    
updateRrdpMeta :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RepositoryStore s -> (SessionId, RrdpSerial) -> RrdpURL -> m ()
updateRrdpMeta tx RepositoryStore {..} meta url = liftIO $ 
    M.get tx rrdpS url >>= \case    
        Nothing -> pure ()
        Just r  -> M.put tx rrdpS url (r { rrdpMeta = Just meta })    


applyChangeSet :: (MonadIO m, Storage s) => 
                Tx s 'RW -> 
                RepositoryStore s -> 
                ChangeSet -> 
                m ()
applyChangeSet tx RepositoryStore {..} (ChangeSet rrdpChanges rsyncChanges lastSucceded) = 
    liftIO $ do
        -- Do the Remove first and only then Put
        let (rrdpPuts, rrdpRemoves) = separate rrdpChanges        

        forM_ rrdpRemoves $ \RrdpRepository{..} ->
                M.delete tx rrdpS uri 
        forM_ rrdpPuts $ \r@RrdpRepository{..} ->
                M.put tx rrdpS uri r

        let (rsyncPuts, rsyncRemoves) = separate rsyncChanges

        forM_ rsyncRemoves $ \(uri', _) ->
                M.delete tx rsyncS uri'
        forM_ rsyncPuts $ \(uri', p) ->
                M.put tx rsyncS uri' p

        let (lastSPuts, lastSRemoves) = separate lastSucceded
        forM_ lastSRemoves $ \(uri', _) -> 
                M.delete tx lastS uri'                
        forM_ lastSPuts $ \(uri', p) ->
                M.put tx lastS uri' p                
    where
        separate = foldr f ([], [])
            where 
                f (Put r) (ps, rs)    = (r : ps, rs) 
                f (Remove r) (ps, rs) = (ps, r : rs) 

getPublicationPoints :: (MonadIO m, Storage s) => 
                        Tx s mode -> RepositoryStore s -> m PublicationPoints
getPublicationPoints tx RepositoryStore {..} = liftIO $ do 
    rrdps <- M.all tx rrdpS 
    rsyns <- M.all tx rsyncS 
    lasts <- M.all tx lastS
    pure $ PublicationPoints 
            (RrdpMap $ Map.fromList rrdps) 
            (RsyncMap $ Map.fromList rsyns)
            (EverSucceededMap $ Map.fromList lasts)

savePublicationPoints :: (MonadIO m, Storage s) => 
                        Tx s 'RW -> RepositoryStore s -> PublicationPoints -> m ()
savePublicationPoints tx store@RepositoryStore {..} newPPs = do 
    ppsInDb <- getPublicationPoints tx store
    let changes = changeSet ppsInDb newPPs
    applyChangeSet tx store changes 
