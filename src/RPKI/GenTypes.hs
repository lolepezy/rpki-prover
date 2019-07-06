module RPKI.GenTypes where

import Data.Kind (Type, Constraint)

-- Some type-level magic used in other modules

type family All (c :: Type -> Constraint) (xs :: [*]) :: Constraint
type instance All c '[]       = ()
type instance All c (x ': xs) = (c x, All c xs)

type family AllF (c :: Type -> Constraint) (m :: x -> f) (xs :: [t]) :: Constraint
type instance AllF c f '[]       = ()
type instance AllF c f (x ': xs) = (c (f x), AllF c f xs)
