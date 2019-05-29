{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module RPKI.BottomUp where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad

import qualified Data.ByteString                   as B
import qualified Data.DList                        as DL
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe                        (maybe)
import qualified Data.Text                         as T

import           Data.Data                         (Typeable)
import           Data.Kind                         (Type)

import           Control.Monad.Trans.Writer.Strict

import qualified StmContainers.Map                 as SM

import           Control.Concurrent.STM.TQueue

import           RPKI.Cache
import           RPKI.Domain

{-
  Bottom-up validation.

  - Find all ROAs
  - Build hierarchy from ROAs up
    * for each ROA find the latest valid CER, memorize
    * validate agains the parent (that means going recursively up)
    * find its effective resource set, memorize
    * for it find the latest valid MFT, memorize
    * on it find the latest valid CRL, memorize
    * if the ROA is not on CRL and
      # all the crypto is fine
      # resource set is in the effective set
      then ROA is valid
    * if the ROA is on the CRL - invalidate

  - For every incoming change
  - for every deleted or updated object invalide it and its referring MFT
  - for every CER, invalidate the whole tree under it, mark all the referred ROAs
    as requiring revalidation
-}

newtype CacheNode = CacheNode (Either CANode RoaNode)

data Cell a = Hole | Loading | Something a
  deriving (Show, Eq, Ord, Typeable)

data CANode = CANode {
  cer            :: !(TVar (Cell CerObject)),
  latestValidMft :: !(TVar (Cell MftObject)),
  latestValidCrl :: !(TVar (Cell CrlObject)),
  chidlren       :: !(SM.Map SKI CacheNode),
  parent         :: !(TVar CANode)
}

data RoaNode = RoaNode {
  roa    :: !(TVar (Cell RoaObject)),
  parent :: !(TVar (Cell CANode))
}

data TAState = TAState {
  -- TODO Replace it with a container allowing for a fast search of ROAs by prefix
  roas  :: [RoaNode],
  store :: Store
}

data Validity = Valid | Invalid

-- small thing to control where we do IO and where not
data InM a b = InIO a | InSTM b

-- validateRoa :: TAState -> RoaNode -> STM Validity
-- validateRoa state r = do
--   readTVar (roa r) >>= \case
--     Something r -> validateWithParent r
--     -- Hole    -> do
--     --   validateWithParent
--   where
--     validateWithParent = do
--       ca@CANode{..} <- parentCA r
--       case findOnCrl r of
--         Nothing ->
--           case (prefix r) `subsetOf` (effectiveResorceSet p) of
--             True  -> pure Valid
--             False -> pure Invalid -- also complain
--         Just onOnCrl -> -- it's revoked
--           -- complain
--           writeTVar r Hole
--           pure Invalid

--     parentCA (RoaNode{..}) = do
--       callValue parent (load store (aki meta)) >>= \case
--         InSTM Nothing -> pure Nothing
--         InSTM certificates -> do
--           Just ca
--         InIO io -> pure $ InIO io


-- callValue :: TVar (Cell c) -> IO (Maybe c) -> STM (InM (IO (Maybe c)) (Maybe c))
-- callValue cell load = do
--   readTVar cell >>= \case
--     Loading     -> retry
--     Something v -> pure $ InSTM $ Just v
--     Hole        -> do
--       writeTVar cell Loading
--       pure $ InIO load


-- -- TODO Add some composition to InM
-- loadCA store aki CANode{..} = do
--   cer' <- callValue cer $ findLatestValid <$> loadByAki
--   mft' <- callValue latestValidMft $ findLatestValidMft <$> loadByAki
--   crl' <- callValue latestValidCrl $ findLatestValidCrl mft <$> loadByAki
--   pure ()
--   where
--     loadByAki = loadLatest store aki

