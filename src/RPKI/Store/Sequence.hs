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
    current <- M.get tx s sequenceName
    let next = Sequence $ maybe 1 (\(Sequence n) -> n + 1) current
    M.put tx s sequenceName next
    pure next