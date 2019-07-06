{-# LANGUAGE RecordWildCards       #-}

module RPKI.BottomUp where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Exception

import qualified Data.ByteString                   as B
import qualified Data.DList                        as DL
import qualified Data.List                         as L
import qualified Data.Map                          as M
import qualified Data.Text                         as T

import Data.Traversable (for)

import           Data.Data                         (Typeable)
import           Data.Maybe (listToMaybe, catMaybes)
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

newtype ANode = ANode (Either CANode RoaNode)

data Cell a = Hole | Loading | Something a | DoNotExist | ShitHappened
  deriving (Show, Eq, Ord, Typeable)

data CANode = CANode {
  latestValidCer :: !(TVar (Cell CerObject)),
  latestValidMft :: !(TVar (Cell MftObject)),
  latestValidCrl :: !(TVar (Cell CrlObject)),
  chidlren       :: !(SM.Map SKI ANode),
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
--       cellValue parent (load store (aki meta)) >>= \case
--         InSTM Nothing -> pure Nothing
--         InSTM certificates -> do
--           Just ca
--         InIO io -> pure $ InIO io


cellValue :: TVar (Cell c) -> IO (Maybe c) -> IO (Maybe c)
cellValue cell io = join $ atomically $ 
  readTVar cell >>= \case
    Loading     -> retry
    Something v -> pure $ pure $ Just v
    Hole        -> do
      writeTVar cell Loading
      pure $ try io >>= \case
          Left (e :: SomeException) -> do
            atomically $ writeTVar cell ShitHappened
            pure Nothing
          Right (Just v) -> do
            atomically $ writeTVar cell $ Something v
            pure $ Just v
          Right Nothing -> do
            atomically $ writeTVar cell DoNotExist
            pure Nothing
 

loadCA store aki CANode{..} = do
  cellValue latestValidCer $ do
    certificates <- niceOnes findCertByDateDesc
    manifests    <- niceOnes findMftByDateDesc
    
    -- find a the most recent pair of MFT and CRL that would be valid 
    cellValue latestValidMft $ do
          n <- for manifests $ \mft -> do
                crls <- cellValue latestValidCrl $
                          listToMaybe <$> (niceOnes $ findCrlByDateDesc mft)                
                pure $ const mft <$> crls                                        
          pure $ listToMaybe $ catMaybes n

    pure $ listToMaybe certificates

  where
    hasValidCrypto _ = True
    findCertByDateDesc = pure $ Just []
    findMftByDateDesc = pure $ Just []
    findCrlByDateDesc _ = pure $ Just []

    niceOnes :: IO (Maybe [a]) -> IO [a]
    niceOnes x = filter hasValidCrypto <$> (concat <$> x)


-- getParent :: ANode -> IO CANode
-- getParent ANode (Left CANode{..}) = do
    
-- getParent ANode (Left CANode{..}) = do


  

