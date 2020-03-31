{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FunctionalDependencies     #-}

module RPKI.Env where

import GHC.Exts (Constraint)
import Data.Kind (Type)
import Data.Data (Typeable)
import Data.Proxy

import GHC.Generics    

import RPKI.Logging
import RPKI.Config


type family WithAll (cs :: [Type]) (t :: Type) :: Constraint where
    WithAll '[] t      = ()
    WithAll (c ': cs') t = (With_ c t, WithAll cs' t)

data TSet (ts :: [*]) where
    TNil  :: TSet '[]
    (:+:) :: t -> TSet ts -> TSet (t ': ts)

infixr 5 :+:

-- -- newtype Env (ts :: [Type]) = Env (TSet ts)

-- data Env (xs :: [Type]) where
--     Env :: WithAll xs (TSet xs) => TSet xs -> Env xs 

class With_ part whole where
    get_ :: whole -> part

instance {-# OVERLAPPING #-} With_ t (TSet (t ': ts)) where
    get_ (t :+: _) = t

instance With_ t (TSet ts) => With_ t (TSet (t1 ': ts)) where
    get_ (_ :+: tx) = get_ tx

-- instance With_ t (TSet ts) => With_ t (Env ts) where
--     get_ (Env ts) = get_ ts


class MappedMono part whole where
    map_ :: (part -> part) -> whole -> whole

instance {-# OVERLAPPING #-} MappedMono t (TSet '[]) where
    map_ _ _ = TNil

instance {-# OVERLAPPING #-} MappedMono t (TSet (t ': ts)) where
    map_ f (t :+: ts) = (f t) :+: ts

-- instance {-# OVERLAPPING #-} MappedMono t (TSet ts) => MappedMono t (TSet (t1 ': ts)) where
--     map_ f (t1 :+: ts) = t1 :+: map_ f ts    

-- get :: With_ t (TSet e) => Env e -> t
-- get (Env e) = get_ e

-- addE :: Env ts -> t -> Env (t ': ts)
-- addE (Env ts) t = Env $ t :+: ts

-- mapE :: MappedMono t (TSet e) => (t -> t) -> Env e -> Env e
-- mapE f (Env e) = Env $ map_ f e

-- makeEnv :: a -> Env '[a]
-- makeEnv a = Env $ a :+: TNil

-- class Tupled t where
--     type Result t :: Type
--     fromTuple :: t -> Result t
--     toTuple :: Result t -> t

-- instance Tupled (a0, a1) where
--     type Result (a0, a1) = TSet '[a0, a1]
--     fromTuple (a0, a1) = a0 :+: a1 :+: TNil
--     toTuple (a0 :+: a1 :+: TNil) = (a0, a1)

-- instance Tupled (a0, a1, a2) where
--     type Result (a0, a1, a2) = TSet '[a0, a1, a2]
--     fromTuple (a0, a1, a2) = a0 :+: a1 :+: a2 :+: TNil
--     toTuple (a0 :+: a1 :+: a2 :+: TNil) = (a0, a1, a2)

-- instance Tupled (a0, a1, a2, a3) where
--     type Result (a0, a1, a2, a3) = TSet '[a0, a1, a2, a3]
--     fromTuple (a0, a1, a2, a3) = a0 :+: a1 :+: a2 :+: a3 :+: TNil
--     toTuple (a0 :+: a1 :+: a2 :+: a3 :+: TNil) = (a0, a1, a2, a3)


data AppContext = AppContext {
    logger :: AppLogger, 
    config :: Config,
    rsyncConf :: RsyncConf
}