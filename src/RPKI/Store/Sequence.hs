{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.Sequence where

import           Codec.Serialise
import           GHC.Generics
import           Data.Text (Text)

import           RPKI.Store.Base.Map      (SMap (..))

import qualified RPKI.Store.Base.Map      as M
import           RPKI.Store.Base.Storage

newtype Sequence = Sequence Integer
    deriving (Show, Eq, Generic, Serialise)

data SequenceStore s = SequenceStore {
    sequences  :: SMap "sequences" s Text Sequence
}

instance Storage s => WithStorage s (SequenceStore s) where
    storage (SequenceStore s) = storage s

nextValue :: Storage s => Tx s 'RW -> SequenceStore s -> Text -> IO Sequence
nextValue tx (SequenceStore s) sequenceName = do
    M.get tx s sequenceName >>= \case
        Nothing -> do
            M.put tx s sequenceName $ Sequence 1
            pure $ Sequence 1
        Just (Sequence number) -> do
            M.put tx s sequenceName $ Sequence $ number + 1
            pure $ Sequence $ number + 1