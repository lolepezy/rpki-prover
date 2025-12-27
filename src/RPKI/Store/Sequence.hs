{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module RPKI.Store.Sequence where
    
import           Data.Text               (Text)
import           GHC.Generics

import           Data.Int

import           RPKI.Store.Base.Map     (SMap (..))

import qualified RPKI.Store.Base.Map     as M
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Serialisation

newtype SequenceValue = SequenceValue { unSequenceValue :: Int64 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary)

nextS :: SequenceValue -> SequenceValue
nextS (SequenceValue n) = SequenceValue (n + 1)

type SequenceMap s = SMap "sequences" s Text SequenceValue

data Sequence s = Sequence {
        sequenceName :: Text,
        sequenceMap :: SMap "sequences" s Text SequenceValue
    } 
    deriving stock (Generic)

instance Storage s => WithStorage s (Sequence s) where
    storage (Sequence _ s) = storage s

nextValue :: Storage s =>             
            Tx s 'RW -> Sequence s -> IO SequenceValue
nextValue tx Sequence {..} = do    
    current <- M.get tx sequenceMap sequenceName
    let next = maybe (SequenceValue 1) nextS current
    M.put tx sequenceMap sequenceName next
    pure next
