{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module RPKI.Types where

import Data.Kind (Type, Constraint)
import Data.Data (Typeable)

-- Some type-level magic used in other modules

type family All (c :: Type -> Constraint) (xs :: [*]) :: Constraint
type instance All c '[]       = ()
type instance All c (x ': xs) = (c x, All c xs)

type family AllF (c :: Type -> Constraint) (m :: x -> f) (xs :: [t]) :: Constraint
type instance AllF c f '[]       = ()
type instance AllF c f (x ': xs) = (c (f x), AllF c f xs)
