module RPKI.Store.Storage where

import RPKI.Domain
import Database.LMDB.Simple

class Storage s where
    type TxM mode s :: * -> *
    getByHash :: forall md . s -> Hash -> TxM md s (Maybe RpkiObject)
    getByAKI :: forall md . s -> AKI -> TxM md s [RpkiObject]
    storeObj :: s -> RpkiObject -> TxM ReadWrite s ()