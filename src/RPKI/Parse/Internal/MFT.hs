module RPKI.Parse.Internal.MFT where

import Control.Monad

import qualified Data.ByteString          as BS
import qualified Data.Text                as Text

import           Data.ASN1.Types
import           Data.Bifunctor (first)
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Time
import           RPKI.Parse.Internal.Common
import           RPKI.Parse.Internal.SignedObject
import qualified RPKI.Util                  as U


parseMft :: BS.ByteString -> PureValidatorT MftObject
parseMft bs = do
    asns      <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' DER bs
    signedMft <- fromEither $ first (parseErr . U.fmtGen) $ 
                    runParseASN1 (parseSignedObject $ parseSignedContent parseManifest) asns
    hash' <- getMetaFromSigned signedMft bs
    pure $ newCMSObject hash' (CMS signedMft)
    where
        {-
            Manifest ::= SEQUENCE {
                version     [0] INTEGER DEFAULT 0,
                manifestNumber  INTEGER (0..MAX),
                thisUpdate      GeneralizedTime,
                nextUpdate      GeneralizedTime,
                fileHashAlg     OBJECT IDENTIFIER,
                fileList        SEQUENCE SIZE (0..MAX) OF FileAndHash }

            https://www.rfc-editor.org/rfc/rfc9286#section-4.2
        -}
        parseManifest :: ParseASN1 Manifest
        parseManifest = onNextContainer Sequence $ do
            parseVersion
            manifestNumber  <- getInteger pure "Wrong manifest number"
            thisUpdateTime' <- getTime "No ThisUpdate time"
            nextUpdateTime' <- getTime "No NextUpdate time"
            hashAlg_        <- getOID asSha256Only "Wrong hash algorithm OID"
            entries         <- getEntries
            -- TODO translate to UTC
            mn        <- makeMftNumber manifestNumber
            thisUpdate <- makeInstant "thisUpdate" thisUpdateTime'
            nextUpdate <- makeInstant "nextUpdate" nextUpdateTime'
            pure $ Manifest mn hashAlg_ thisUpdate nextUpdate entries

        -- Reject times that `Instant` cannot represent rather than wrapping them
        makeInstant what t = 
            maybe (throwParseError $ "Manifest " <> what <> " is out of the representable range: " <> show t) 
                  pure (newInstantChecked t)

        -- The version is `[0] EXPLICIT INTEGER DEFAULT 0`. DER requires DEFAULT 
        -- values to be omitted, but accept an explicitly encoded 0 as well -- 
        -- unlike the value 1, which the previous version of this parser required 
        -- from an untagged integer that could never appear here in the first place.
        parseVersion =
            getNextContainerMaybe (Container Context 0) >>= \case
                Nothing          -> pure ()
                Just [IntVal 0]  -> pure ()
                Just s           -> throwParseError $ "Unexpected manifest version: " ++ show s

        makeMftNumber n = either throwParseError pure $ makeSerial n

        getEntries = onNextContainer Sequence $
            getMany $ onNextContainer Sequence $
                MftPair <$> getIA5String (pure . Text.pack) "Wrong file name"
                        <*> getBitString (pure . U.mkHash) "Wrong hash"
