module RPKI.Parse.Internal.MFT where

import Control.Monad

import qualified Data.ByteString          as BS
import qualified Data.Text                as Text
import qualified Data.Vector              as V

import           Data.ASN1.Types
import           Data.Bifunctor (first)
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.Tuple.Strict

import           RPKI.Domain
import           RPKI.CommonTypes
import           RPKI.Time
import           RPKI.Parse.Internal.Common
import           RPKI.Parse.Internal.SignedObject
import           RPKI.Util (mkHash)


parseMft :: BS.ByteString -> ParseResult (RpkiURL -> MftObject)
parseMft bs = do
    asns         <- first (fmtErr . show) $ decodeASN1' BER bs
    signedMft    <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseManifest) asns
    meta <- getMetaFromSigned signedMft bs
    pure $ \url -> let (hash, loc) = meta url in newCMSObject hash loc (CMS signedMft)
    where
        parseManifest :: ParseASN1 Manifest
        parseManifest = onNextContainer Sequence $
            (,,) <$> getNext <*> getNext <*> getNext >>= \case
                    (IntVal manifestNumber,
                        ASN1Time TimeGeneralized thisUpdateTime' _,
                        ASN1Time TimeGeneralized nextUpdateTime' _) -> do
                            hashAlg' <- getOID oid2Hash "Wrong hash algorithm OID"
                            entries <- getEntries fileHashAlg
                            -- TODO translate to UTC
                            pure $ Manifest (fromInteger manifestNumber) hashAlg' 
                                (Instant thisUpdateTime') (Instant nextUpdateTime') entries

                    -- TODO Check version?
                    (IntVal version,
                        IntVal manifestNumber,
                        ASN1Time TimeGeneralized thisUpdateTime' _) -> do
                            when (version /= 1) $ 
                                throwParseError $ "Unexpected ROA content: " ++ show version
                            nextUpdateTime' <- getTime "No NextUpdate time"
                            hashAlg'        <- getOID oid2Hash "Wrong hash algorithm OID"
                            entries         <- getEntries fileHashAlg
                            -- TODO translate to UTC
                            pure $ Manifest (fromInteger manifestNumber) hashAlg' 
                                (Instant thisUpdateTime') (Instant nextUpdateTime') entries

                    s -> throwParseError $ "Unexpected ROA content: " ++ show s

        getEntries _ = onNextContainer Sequence $
            getMany $ onNextContainer Sequence $
                T2 <$> getIA5String (pure . Text.pack) "Wrong file name"
                   <*> getBitString (pure . mkHash) "Wrong hash"

        getTime message = getNext >>= \case
            ASN1Time TimeGeneralized dt _ -> pure dt
            s  -> throwParseError $ message ++ ", got " ++ show s


