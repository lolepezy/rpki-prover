{-# LANGUAGE LambdaCase #-}
module RPKI.Binary.ASN1Util where
  
import Data.ASN1.Types
import Data.ASN1.Parse

getNull :: ParseASN1 a -> ParseASN1 a
getNull f = getNext >>= \case 
      Null -> f
      b    -> throwParseError $ "Unexpected: " ++ show b