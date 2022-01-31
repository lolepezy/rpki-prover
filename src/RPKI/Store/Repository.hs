{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Repository where

import           Control.Monad.IO.Class

import           Data.Foldable (for_)
import qualified Data.Map.Strict          as Map

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Repository
import           RPKI.Store.Base.Map      (SMap (..))
import qualified RPKI.Store.Base.Map      as M
import           RPKI.Store.Base.Storage


data RepositoryStore s = RepositoryStore {
    rrdpS  :: SMap "rrdp-repositories" s RrdpURL RrdpRepository,
    rsyncS :: SMap "rsync-repositories" s RsyncHost RsyncNodeNormal,
    lastS  :: SMap "last-fetch-success" s RpkiURL FetchEverSucceeded
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _) = storage s


updateRrdpMeta :: (MonadIO m, Storage s) =>
                Tx s 'RW -> RepositoryStore s -> (SessionId, RrdpSerial) -> RrdpURL -> m ()
updateRrdpMeta tx RepositoryStore {..} meta url = liftIO $ do
    z <- M.get tx rrdpS url
    for_ z $ \r -> M.put tx rrdpS url (r { rrdpMeta = Just meta })


applyChangeSet :: (MonadIO m, Storage s) =>
                Tx s 'RW ->
                RepositoryStore s ->
                ChangeSet ->
                m ()
applyChangeSet tx RepositoryStore {..} (ChangeSet rrdpChanges rsyncChanges lastSucceded) =
    liftIO $ do
        -- Do the Remove first and only then Put
        let (rrdpPuts, rrdpRemoves) = separate rrdpChanges

        for_ rrdpRemoves $ \RrdpRepository{..} -> M.delete tx rrdpS uri
        for_ rrdpPuts $ \r@RrdpRepository{..}  -> M.put tx rrdpS uri r

        let (rsyncPuts, rsyncRemoves) = separate rsyncChanges

        for_ rsyncRemoves $ \(uri', _) -> M.delete tx rsyncS uri'
        for_ rsyncPuts $ uncurry (M.put tx rsyncS)

        let (lastSPuts, lastSRemoves) = separate lastSucceded
        for_ lastSRemoves $ \(uri', _) -> M.delete tx lastS uri'
        for_ lastSPuts $ uncurry (M.put tx lastS)
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
            (RsyncTree $ Map.fromList rsyns)
            (EverSucceededMap $ Map.fromList lasts)

savePublicationPoints :: (MonadIO m, Storage s) =>
                        Tx s 'RW -> RepositoryStore s -> PublicationPoints -> m ()
savePublicationPoints tx store@RepositoryStore {} newPPs' = do
    ppsInDb <- getPublicationPoints tx store
    let changes = changeSet ppsInDb newPPs'
    applyChangeSet tx store changes
