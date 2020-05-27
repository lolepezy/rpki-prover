{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RPKI.Store.Sequence where

import           Codec.Serialise
import           Data.Text               (Text)
import           GHC.Generics

import           Data.Int

import           RPKI.Store.Base.Map     (SMap (..))

import qualified RPKI.Store.Base.Map     as M
import           RPKI.Store.Base.Storage

newtype SequenceValue = SequenceValue Int64
    deriving (Show, Eq, Generic, Serialise)

data Sequence s = Sequence {
    sequenceName :: Text,
    sequences :: SMap "sequences" s Text SequenceValue
}

instance Storage s => WithStorage s (Sequence s) where
    storage (Sequence _ s) = storage s

nextValue :: Storage s =>             
            Tx s 'RW -> Sequence s -> IO SequenceValue
nextValue tx Sequence {..} = do    
    current <- M.get tx sequences sequenceName
    let next = SequenceValue $ maybe 1 (\(SequenceValue n) -> n + 1) current
    M.put tx sequences sequenceName next
    pure next
