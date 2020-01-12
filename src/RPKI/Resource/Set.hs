{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Resource.Set where

import           Codec.Serialise

import Prelude hiding (elem, filter)

import           Data.Data              (Typeable)
import           Data.List (nub)
import           Data.Kind
import           Data.Maybe (isJust)
import qualified Data.Vector            as V
import           GHC.Generics

import           RPKI.Resource.Resource

data ValidationRFC = Strict_ | Reconsidered_
    deriving (Show, Eq, Ord, Typeable, Generic)

data SmallSet a = SS !(V.Vector a)
  deriving (Show, Eq, Ord, Typeable, Generic)

instance Serialise a => Serialise (SmallSet a)

data RSet r = RS !r | Inherit
    deriving (Show, Eq, Ord, Typeable, Generic)

data IpSet = IpSet
    !(RSet (SmallSet Ipv4Prefix))
    !(RSet (SmallSet Ipv6Prefix))
    deriving (Show, Eq, Ord, Typeable, Generic)

instance Serialise IpSet
instance Serialise r => Serialise (RSet r)

newtype IpResources (rfc :: ValidationRFC) = IpResources (IpSet)
    deriving (Show, Eq, Ord, Typeable, Generic)

newtype AsResources (rfc :: ValidationRFC) = AsResources (RSet (SmallSet AsResource))
    deriving (Show, Eq, Ord, Typeable, Generic)


fromList :: Eq a => [a] -> SmallSet a
fromList = SS . V.fromList . nub

toList :: SmallSet a -> [a]
toList (SS v) = V.toList v

empty :: Eq a => SmallSet a
empty = fromList []

emptyIpSet :: IpSet
emptyIpSet = IpSet (RS empty) (RS empty)

find :: (a -> Bool) -> SmallSet a -> Maybe a
find p (SS v) = V.find p v    

filter :: (a -> Bool) -> SmallSet a -> SmallSet a
filter p (SS v) = SS $ V.filter p v    

elem :: Eq a => a -> SmallSet a -> Bool   
elem a = isJust . find (==a)


data ResourseCheck = NestedReconsidered | Overclaiming { 
    interesection :: !IpSet,
    overclaiming :: !IpSet   
}

data ResourseCheckStrict = NestedStrict | ResourseCheckStrict { overclaiming :: !IpSet }

class ResourceCheck (rfc :: ValidationRFC) where
    type Check rfc :: Type
    check :: IpResources rfc -> IpResources rfc -> Check rfc


-- TODO Implement resource set subtraction
subsetStrict :: IpResources 'Strict_ -> IpResources 'Strict_ -> ResourseCheckStrict
subsetStrict smaller@(IpResources (IpSet s4 s6)) (IpResources (IpSet b4 b6)) = 
    case (checkSet s4 b4, checkSet s6 b6) of
        (Nothing, Nothing) -> NestedStrict
        (overV4, overV6)   -> ResourseCheckStrict $ IpSet (toRS overV4) (toRS overV6)                
    where
        toRS = maybe (RS empty) RS            
        checkSet s b = case (s, b) of
            (Inherit, Inherit) -> Nothing
            (RS s, Inherit)    -> Just s
            (Inherit, RS _)    -> Nothing
            (RS ss, RS bs)     -> Just $ filter notInBigOne ss
                where 
                    notInBigOne p = not $ isJust $ find (\bp -> p `contains` bp) bs

