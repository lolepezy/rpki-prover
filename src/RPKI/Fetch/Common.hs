{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Fetch.Common where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class

import           Data.IORef
import           Data.Data
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           Data.Foldable                   (for_)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map            
import qualified Data.Map.Monoidal.Strict        as MonoidalMap     
import           Data.IxSet.Typed                (IxSet, Indexable, IsIndexOf, ixFun, ixList)
import qualified Data.IxSet.Typed                as IxSet

import           GHC.Generics


import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Serialisation
import           RPKI.Parallel


data Fetchers = Fetchers {
        -- All fetchables after the last validation, i.e. repositories
        -- with their fallbacks
        fetcheables :: TVar Fetcheables,

        -- Fetchers that are currently running
        runningFetchers :: TVar (Map RpkiURL ThreadId),        

        -- Version for the first fetch that was finished for every repository.
        -- We want to track this to for the "one-off" mode.
        firstFinishedFetchBy :: TVar (Map RpkiURL WorldVersion),        

        -- Semaphore for untrusted fetches, i.e fetches that have 
        -- no decent history of being successful 
        untrustedFetchSemaphore :: Semaphore,

        -- Semaphore for trusted fetches, i.e. fetches 
        -- that have already succeeded
        trustedFetchSemaphore :: Semaphore,

        -- Semaphore for rsync fetches per host, used to no exceed 
        -- the limit of connections per rsync host
        rsyncPerHostSemaphores  :: TVar (Map RsyncHost Semaphore),

        -- Mapping of repositories to the TAs they are mentioned in
        uriByTa :: TVar UriTaIxSet
    }
    deriving stock (Generic)

type UriTaIxSet = IxSet Indexes UrlTA

data UrlTA = UrlTA RpkiURL TaName
    deriving stock (Show, Eq, Ord, Generic, Data, Typeable)    

type Indexes = '[RpkiURL, TaName]

instance Indexable Indexes UrlTA where
    indices = ixList
        (ixFun (\(UrlTA url _) -> [url]))
        (ixFun (\(UrlTA _ ta)  -> [ta]))        

data Fetched = Fetched {
        repository :: Repository,
        hasUpdates :: Bool,
        rrdpStats  :: Maybe RrdpFetchStat
    }
    deriving stock (Show, Eq, Ord, Generic, Typeable)    

data Update = ObjectUpdate (Vector AddedObject)
            | TaUpdate TaName
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype AddedObject = AddedObject ObjectKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


withUpdateAccum :: MonadIO m 
                => RpkiURL 
                -> ((ObjectKey -> m ()) -> (m (Vector AddedObject)) -> m a)
                -> m (a, Bool)
withUpdateAccum url action = do
    buffer <- liftIO $ newIORef []
    count  <- liftIO $ newIORef (0 :: Int)

    let addObject objectKey = liftIO $ do
            n <- readIORef count
            writeIORef count $! n + 1    
            modifyIORef' buffer (AddedObject objectKey :)            

    let getUpdates = liftIO $ V.fromList <$> readIORef buffer

    result <- action addObject getUpdates    
    n      <- liftIO $ readIORef count    
    pure (result, n > 0)


deleteByIx :: (Indexable ixs a, IsIndexOf ix ixs) => ix -> IxSet ixs a -> IxSet ixs a
deleteByIx ix_ s = foldr IxSet.delete s $ IxSet.getEQ ix_ s

dropFetcher :: Fetchers -> RpkiURL -> IO ()
dropFetcher Fetchers {..} url = mask_ $ do
    readTVarIO runningFetchers >>= \running -> do
        for_ (Map.lookup url running) $ \thread -> do
            Conc.throwTo thread AsyncCancelled
            atomically $ do
                modifyTVar' runningFetchers $ Map.delete url
                modifyTVar' uriByTa $ deleteByIx url

updateUriPerTa :: Map TaName Fetcheables -> UriTaIxSet -> UriTaIxSet
updateUriPerTa fetcheablesPerTa uriTa = uriTa'
  where 
    -- TODO Optimise it
    cleanedUpPerTa = foldr deleteByIx uriTa $ Map.keys fetcheablesPerTa        

    uriTa' = 
        IxSet.insertList [ UrlTA url ta | 
                (ta, Fetcheables fs) <- Map.toList fetcheablesPerTa,
                url <- MonoidalMap.keys fs
            ] cleanedUpPerTa 

