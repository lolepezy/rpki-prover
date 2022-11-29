{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.Sequence where
    
import           Data.Text               (Text)
import           GHC.Generics

import           Data.Int

import           RPKI.Store.Base.Map     (SMap (..))

import qualified RPKI.Store.Base.Map     as M
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Serialisation

newtype SequenceValue = SequenceValue Int64
    deriving (Show, Eq, Generic, TheBinary)

type SequenceMap s = SMap "sequences" s Text SequenceValue

data Sequence s = Sequence {
    sequenceName :: Text,
    sequenceMap :: SMap "sequences" s Text SequenceValue
}

instance Storage s => WithStorage s (Sequence s) where
    storage (Sequence _ s) = storage s

nextValue :: Storage s =>             
            Tx s 'RW -> Sequence s -> IO SequenceValue
nextValue tx Sequence {..} = do    
    current <- M.get tx sequenceMap sequenceName
    let next = SequenceValue $ maybe 1 (\(SequenceValue n) -> n + 1) current
    M.put tx sequenceMap sequenceName next
    pure next
