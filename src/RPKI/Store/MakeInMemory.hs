{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.MakeInMemory where

import           Data.Text (Text)

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           RPKI.Store.Base.InMemory

import RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Sequence



createObjectStore :: SequenceMap InMemoryStorage -> IO (RpkiObjectStore InMemoryStorage)
createObjectStore seqMap =
    RpkiObjectStore 
        (Sequence "object-key" seqMap) <$>
        (SMap InMemoryStorage <$> createMapStore) <*>        
        (SMap InMemoryStorage <$> createMapStore) <*>
        (SMap InMemoryStorage <$> createMapStore) <*>
        (SMap InMemoryStorage <$> createMapStore) <*>
        (SMultiMap InMemoryStorage <$> createMultiMapStore) <*>
        (SMap InMemoryStorage <$> createMapStore) <*>   
        (SMap InMemoryStorage <$> createMapStore)    

createRepositoryStore :: IO (RepositoryStore InMemoryStorage)
createRepositoryStore = 
    RepositoryStore <$>
        (SMap InMemoryStorage <$> createMapStore) <*>
        (SMap InMemoryStorage <$> createMapStore) <*>
        (SMap InMemoryStorage <$> createMapStore)    

createResultStore :: IO (ValidationsStore InMemoryStorage)
createResultStore= 
    ValidationsStore <$> (SMap InMemoryStorage <$> createMapStore)    

createVRPStore :: IO (VRPStore InMemoryStorage)
createVRPStore = VRPStore . SMap InMemoryStorage <$> createMapStore    

createTAStore :: IO (TAStore InMemoryStorage)
createTAStore = TAStore . SMap InMemoryStorage <$> createMapStore    

createVersionStore :: IO (VersionStore InMemoryStorage)
createVersionStore = VersionStore . SMap InMemoryStorage <$> createMapStore    

createMetricsStore :: IO (MetricsStore InMemoryStorage)
createMetricsStore = MetricsStore . SMap InMemoryStorage <$> createMapStore    

createSequenceStore :: Text -> IO (Sequence InMemoryStorage)
createSequenceStore seqName = Sequence seqName . SMap InMemoryStorage <$> createMapStore    

createDatabase :: IO (DB InMemoryStorage)
createDatabase = do 
    seqMap <- SMap InMemoryStorage <$> createMapStore
    DB <$>
        createTAStore <*>
        createRepositoryStore <*>
        createObjectStore seqMap <*>
        createResultStore <*>
        createVRPStore <*>
        createVersionStore <*>
        createMetricsStore <*>
        pure seqMap

