{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}


module RPKI.Store.Base.LMDB where

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception


import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Coerce (coerce)

import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage
import RPKI.Store.Base.Serialisation
import RPKI.Parallel
import RPKI.AppTypes
import RPKI.Util (convert)

import Data.IORef

import GHC.TypeLits
import GHC.Generics
import Data.Proxy as P

import Lmdb.Connection
import Lmdb.Codec (byteString)
import qualified Lmdb.Map as LMap
import qualified Lmdb.Multimap as LMMap
import qualified Lmdb.Types as Lmdb

import Pipes


type Env = Lmdb.Environment 'Lmdb.ReadWrite
type DBMap = Lmdb.Database BS.ByteString BS.ByteString

data NativeEnv = ROEnv Env
               | RWEnv Env
               | Disabled
               | TimedOut

data LmdbEnv = LmdbEnv {
    nativeEnv :: TVar NativeEnv,
    txSem     :: Semaphore
}

data LmdbStore (name :: Symbol) = LmdbStore {
    db  :: DBMap,
    env :: LmdbEnv
}

data LmdbMultiStore (name :: Symbol) = LmdbMultiStore {
    db  :: Lmdb.MultiDatabase BS.ByteString BS.ByteString,
    env :: LmdbEnv
}

type family LmdbTxMode (m :: TxMode) :: Lmdb.Mode where
    LmdbTxMode 'RO = 'Lmdb.ReadOnly
    LmdbTxMode 'RW = 'Lmdb.ReadWrite

toROTx :: Lmdb.Transaction (m :: Lmdb.Mode) -> Lmdb.Transaction 'Lmdb.ReadOnly
toROTx = coerce

newtype LmdbStorage = LmdbStorage { unEnv :: LmdbEnv }

instance WithTx LmdbStorage where
    data Tx LmdbStorage (m :: TxMode) = LmdbTx (Lmdb.Transaction (LmdbTxMode m))

    readOnlyTx lmdb f = let
        e@LmdbEnv {..} = unEnv lmdb
        in withSemaphore txSem $ do
                nEnv <- atomically $ getNativeEnv e
                withROTransaction nEnv (f . LmdbTx)

    readWriteTx lmdb f = withTransactionWrapper (unEnv lmdb) $ \tx -> do 
        -- Re-check that the env is not timed out.
        -- That may happen if multiple threads were waiting 
        -- to start a writing transaction.
        env <- readTVarIO $ nativeEnv (unEnv lmdb)
        case env of 
            TimedOut -> throwIO TxTimeout
            _        -> f $ LmdbTx tx
        

withTransactionWrapper :: LmdbEnv -> (Lmdb.Transaction 'Lmdb.ReadWrite -> IO b) -> IO b
withTransactionWrapper LmdbEnv {..} f = do
    nEnv <- atomically $ do
        readTVar nativeEnv >>= \case
            Disabled     -> retry
            TimedOut     -> retry
            ROEnv _      -> retry
            RWEnv native -> pure native

    withSemaphore txSem $ do
        stillWaiting <- newTVarIO True
        z <- race
            (interruptAfterTimeout stillWaiting)
            (runTx nEnv stillWaiting)
        case z of
            Left _ -> 
                -- this will actually never happen,
                -- so it's just to satisfy the typechecker
                throwIO TxTimeout
            Right x -> pure x
  where
    -- TODO Make it configurable 
    timeoutDuration = 600_000_000

    runTx nEnv stillWaiting =
        withTransaction nEnv $ \tx -> do
            atomically $ writeTVar stillWaiting False
            f tx

    interruptAfterTimeout stillWaiting = do
        threadDelay timeoutDuration
        sw <- readTVarIO stillWaiting        
        when sw $ do            
            atomically $ writeTVar nativeEnv TimedOut
            throwIO TxTimeout            


-- | Basic storage implemented using LMDB
instance Storage LmdbStorage where
    type SMapImpl LmdbStorage = LmdbStore
    type SMultiMapImpl LmdbStorage = LmdbMultiStore

    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) =
        LMap.repsert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        LMap.delete' tx db ks

    clear (LmdbTx tx) LmdbStore {..} =
        LMap.clear tx db

    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        (SValue . Storable <$>) <$> LMap.lookup' (toROTx tx) db ks

    foldS (LmdbTx tx) LmdbStore {..} f a0 =
        foldGeneric tx db f a0 withCursor LMap.firstForward

    putMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable vs)) =
        withMultiCursor tx db $ \c -> LMMap.insert c ks vs

    deleteMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable vs)) =
        LMMap.deleteKV tx db ks vs

    deleteAllMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) =
        withMultiCursor tx db $ \c ->
            LMMap.lookupFirstValue c ks >>= \case
                Nothing -> pure ()
                Just _  -> LMMap.deleteValues c

    clearMu (LmdbTx tx) LmdbMultiStore {..} =
        LMMap.clear tx db

    foldMuForKey (LmdbTx tx) LmdbMultiStore {..} key@(SKey (Storable ks)) f a0 =
        withMultiCursor tx db $ \c -> do
            z <- newIORef a0
            runEffect $ LMMap.lookupValues c ks >-> do
                forever $ do
                    v <- await
                    lift $ do
                        a <- readIORef z
                        !a' <- f a key (SValue $ Storable v)
                        writeIORef z a'
            readIORef z

    foldMu (LmdbTx tx) LmdbMultiStore {..} f a0 =
        foldGeneric tx db f a0 withMultiCursor LMMap.firstForward

foldGeneric :: forall tx db cursor a .
            tx
        -> db
        -> (a -> SKey -> SValue -> IO a)
        -> a
        -> (tx -> db -> (cursor -> IO a) -> IO a)
        -> (cursor -> Producer' (Lmdb.KeyValue BS.ByteString BS.ByteString) IO ())
        -> IO a
foldGeneric tx db f a0 withCurs makeProducer =
    withCurs tx db $ \c -> do
        z <- newIORef a0
        void $ runEffect $ makeProducer c >-> do
            forever $ do
                Lmdb.KeyValue k v <- await
                lift $ do
                    a <- readIORef z
                    !a' <- f a (SKey $ Storable k) (SValue $ Storable v)
                    writeIORef z a'
        readIORef z


createLmdbStore :: forall name . KnownSymbol name =>
                    LmdbEnv -> IO (LmdbStore name)
createLmdbStore env@LmdbEnv {} = do
    let name' = symbolVal (P.Proxy @name)
    db <- withTransactionWrapper env $ \tx -> openDatabase tx (Just name') defaultDbSettings
    pure $ LmdbStore db env

createLmdbMultiStore :: forall name . KnownSymbol name =>
                        LmdbEnv -> IO (LmdbMultiStore name)
createLmdbMultiStore env@LmdbEnv {} = do
    let name' = symbolVal (P.Proxy @name)
    db <- withTransactionWrapper env $ \tx -> openMultiDatabase tx (Just name') defaultMultiDbSettngs
    pure $ LmdbMultiStore db env


defaultDbSettings :: Lmdb.DatabaseSettings BS.ByteString BS.ByteString
defaultDbSettings = makeSettings
    (Lmdb.SortNative Lmdb.NativeSortLexographic)
    byteString byteString

defaultMultiDbSettngs :: Lmdb.MultiDatabaseSettings BS.ByteString BS.ByteString
defaultMultiDbSettngs = makeMultiSettings
    (Lmdb.SortNative Lmdb.NativeSortLexographic)
    (Lmdb.SortNative Lmdb.NativeSortLexographic)
    byteString byteString


getNativeEnv :: LmdbEnv -> STM Env
getNativeEnv LmdbEnv {..} = do
    readTVar nativeEnv >>= \case
        Disabled     -> retry
        TimedOut     -> retry
        ROEnv native -> pure native
        RWEnv native -> pure native

disableNativeEnv :: LmdbEnv -> STM ()
disableNativeEnv LmdbEnv {..} =
    writeTVar nativeEnv Disabled

data CopyStat = CopyStat {
        mapName       :: BS.ByteString,
        totalSize     :: Int,
        maxKVPairSize :: Int
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)


-- | Copy all databases from one LMDB environment to the other
-- This is a low-level operation to be used for de-fragmentation.
-- `dstN` is supposed to be a completely empty environment.
--
copyEnv :: Env -> Env -> IO [CopyStat]
copyEnv srcN dstN = do
    withROTransaction srcN $ \srcTx -> do
        withTransaction dstN $ \dstTx -> do
            srcDb <- openDatabase srcTx Nothing defaultDbSettings
            mapNames <- getMapNames srcTx srcDb
            forM mapNames $ \mapName -> do
                -- first open it as is
                srcMap  <- openDatabase srcTx (Just $ convert mapName) defaultDbSettings
                isMulti <- isMultiDatabase srcTx srcMap
                if isMulti
                    then do
                        -- close and reopen as multi map
                        closeDatabase srcN srcMap
                        srcMap' <- openMultiDatabase srcTx (Just $ convert mapName) defaultMultiDbSettngs
                        dstMap  <- openMultiDatabase dstTx (Just $ convert mapName) defaultMultiDbSettngs
                        (copied, biggest) <- copyMultiMap srcMap' dstMap srcTx dstTx
                        pure $! CopyStat mapName copied biggest
                    else do
                        dstMap <- openDatabase dstTx (Just $ convert mapName) defaultDbSettings
                        (copied, biggest) <- copyMap srcMap dstMap srcTx dstTx
                        pure $! CopyStat mapName copied biggest
  where
    copyMap srcMap dstMap srcTx dstTx =
        withKVs $ \bytes maxKV -> do
            withCursor srcTx srcMap $ \c ->
                void $ runEffect $ LMap.firstForward c >-> do
                    forever $ do
                        Lmdb.KeyValue name value <- await
                        void $ lift $ LMap.insertSuccess' dstTx dstMap name value
                        let kvSize = BS.length name + BS.length value
                        lift $ do
                            modifyIORef' bytes ( + kvSize)
                            modifyIORef' maxKV (max kvSize)

    copyMultiMap srcMap dstMap srcTx dstTx =
        withKVs $ \bytes maxKV -> do
            withMultiCursor dstTx dstMap $ \dstC ->
                withMultiCursor srcTx srcMap $ \srcC ->
                    void $ runEffect $ LMMap.firstForward srcC >-> do
                        forever $ do
                            Lmdb.KeyValue name value <- await
                            lift $ LMMap.insert dstC name value
                            let kvSize = BS.length name + BS.length value
                            lift $ do
                                modifyIORef' bytes ( + kvSize)
                                modifyIORef' maxKV (max kvSize)

    withKVs processThem = do
        bytes <- newIORef 0
        maxKV <- newIORef 0
        void $ processThem bytes maxKV
        (,) <$> readIORef bytes <*> readIORef maxKV


getEnvStats :: Env -> IO StorageStats
getEnvStats env =
    fmap (StorageStats . Map.fromList) $
        withROTransaction env $ \tx -> do
            db <- openDatabase tx Nothing defaultDbSettings
            mapNames <- getMapNames tx db
            forM mapNames $ \mapName -> do
                -- first open it as is
                m <- openDatabase tx (Just $ convert mapName) defaultDbSettings
                isMulti <- isMultiDatabase tx m
                stat <- if isMulti
                        then do
                            -- close and reopen as multi map
                            closeDatabase env m
                            m' <- openMultiDatabase tx (Just $ convert mapName) defaultMultiDbSettngs
                            gatherStats tx m' withMultiCursor LMMap.firstForward
                        else
                            gatherStats tx m withCursor LMap.firstForward
                pure (convert mapName, stat)
  where
    gatherStats tx m cursorF forwardF = do
        stats <- newIORef mempty
        void $ cursorF tx m $ \c ->
            void $ runEffect $ forwardF c >-> do
                forever $ do
                    Lmdb.KeyValue key value <- await
                    let keySize   = Size $ fromIntegral $ BS.length key
                    let valueSize = Size $ fromIntegral $ BS.length value
                    let stat = SStats {
                                    statSize          = Size 1,
                                    statKeyBytes      = keySize,
                                    statValueBytes    = valueSize,
                                    statMaxKeyBytes   = keySize,
                                    statMaxValueBytes = valueSize
                                }
                    lift $ modifyIORef' stats (<> stat)
        readIORef stats


getMapNames :: Lmdb.Transaction e -> Lmdb.Database a v -> IO [a]
getMapNames tx db =
    withCursor tx db $ \c -> do
        maps <- newIORef []
        void $ runEffect $ LMap.firstForward c >-> do
            forever $ do
                Lmdb.KeyValue name _ <- await
                lift $ modifyIORef' maps (name :)
        readIORef maps


eraseEnv :: Env -> Tx LmdbStorage 'RW -> IO [BS.ByteString]
eraseEnv env (LmdbTx tx) = do
    db <- openDatabase tx Nothing defaultDbSettings
    mapNames <- getMapNames tx db
    forM_ mapNames $ \mapName -> do
        -- first open it as is
        m <- openDatabase tx (Just $ convert mapName) defaultDbSettings
        isMulti <- isMultiDatabase tx m
        if isMulti
            then do
                -- close and reopen as multi map
                closeDatabase env m
                m' <- openMultiDatabase tx (Just $ convert mapName) defaultMultiDbSettngs
                LMMap.clear tx m'
            else
                LMap.clear tx m
    pure mapNames