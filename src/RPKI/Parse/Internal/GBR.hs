{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.GBR where

import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Parse
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import GHC.Generics
import RPKI.AppMonad
import RPKI.Domain
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject
import qualified RPKI.Util as U


-- | Parse Ghostbusters record (https://tools.ietf.org/html/rfc6493)
-- 
parseGbr :: BS.ByteString -> PureValidatorT GbrObject
parseGbr bs = do    
    asns      <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs  
    signedGbr <- fromEither $ first (parseErr . U.fmtGen) $ 
                    runParseASN1 (parseSignedObject parseGbr') asns
    hash' <- getMetaFromSigned signedGbr bs
    pure $ newCMSObject hash' (CMS signedGbr)
    where     
        parseGbr' contentType octets = 
            pure $ EncapsulatedContentInfo contentType (Gbr $ toShortBS octets)


{- 

   BEGIN -  pro forma packaging that MUST be the first line in the vCard
      and MUST have the value "BEGIN:VCARD" as described in [RFC6350].

   VERSION -  pro forma packaging that MUST be the second line in the
      vCard and MUST have the value "VERSION:4.0" as described in
      Section 3.7.9 of [RFC6350].

   FN -  the name, as described in Section 6.2.1 of [RFC6350], of a
      contactable person or role who is responsible for the CA
      certificate.

   ORG -  an organization as described in Section 6.6.4 of [RFC6350].

   ADR -  a postal address as described in Section 6.3 of [RFC6350].

   TEL -  a voice and/or fax phone as described in Section 6.4.1 of
      [RFC6350].

   EMAIL -  an Email address as described in Section 6.4.2 of [RFC6350]

   END -  pro forma packaging that MUST be the last line in the vCard
      and MUST have the value "END:VCARD" as described in [RFC6350].


Bush                         Standards Track                    [Page 4]

 
RFC 6493                RPKI Ghostbusters Record           February 2012


   Per [RFC6350], the BEGIN, VERSION, FN, and END properties MUST be
   included in a record.  To be useful, at least one of ADR, TEL, and
   EMAIL MUST be included.  Other properties MUST NOT be included.

BEGIN:VCARD
VERSION:4.0
FN:Job Snijders
ORG:Sobornost
ADR;TYPE=HOME:;;Theodorus Majofskistraat 100;Amsterdam;;1065 SZ;The Netherlands
TEL;TYPE=VOICE,TEXT,HOME;VALUE=uri:tel:+31-6-54942365
EMAIL:job@sobornost.net
END:VCARD
-}

data VCardProperty = VCardVersion Text | FN Text | ADR Text | TEL Text | EMAIL Text
    deriving (Show, Eq, Ord, Generic)

newtype VCard = VCard [VCardProperty]
    deriving (Show, Eq, Ord, Generic)

-- | Some primitive parsing of VCARD, not based on the proper RFC, 
-- but checking for the necessary fields.
parseVCard :: BS.ByteString -> Either Text (VCard, Maybe Text)
parseVCard bs = do
    t <- first (Text.pack . show) $ TE.decodeUtf8' bs
    let linez = map (Text.split (==':') . Text.strip) $ Text.lines t
    case linez of 
        [["BEGIN","VCARD"]] -> 
            Left "Doesn't contain anything meaningful"
        ["BEGIN","VCARD"] : fs | List.last fs /= ["END","VCARD"] -> 
                                    Left "Doesn't end with END:VCARD"
                               | otherwise -> 
                                    parseFields fs
        whateverElse -> Left $ Text.pack $ 
                            "Doesn't begin with BEGIN:VCARD, begins with " <> show whateverElse    
  where
    parseFields fs = let 
        f_FN    = [ FN  $ mconcat fn | "FN"  : fn <- fs ]
        f_ADR   = [ ADR $ mconcat fn   | adr : fn      <- fs, "ADR" `Text.isPrefixOf` adr ]
        f_TEL   = [ TEL $ mconcat fn   | tel : fn      <- fs, "TEL" `Text.isPrefixOf` tel ]
        f_EMAIL = [ EMAIL $ mconcat fn | "EMAIL" : fn  <- fs ]
        f_OTHER = [ f <> " " <> mconcat fn | f : fn  <- fs, f `elem` ["ORG", "N"]]

        -- Validate according to https://www.rfc-editor.org/rfc/rfc6493.html#section-7
        -- 
        -- Here we intentionally ignore the "Other properties MUST NOT be included" part.
        -- It will make existing GBRs invalid and it's not something anyone wants to care about.
        in case f_FN of
            [] -> Left "FN MUST be included"
            _ | null f_ADR && null f_TEL && null f_EMAIL -> 
                    Left "At least one of ADR, TEL, and EMAIL MUST be included"
              | otherwise -> let
                    warnings = case f_OTHER of
                        [] -> Nothing
                        _  -> Just $ "Other properties MUST NOT be included, but they were '" <> mconcat f_OTHER <> "'"
                    in Right (VCard $ f_FN <> f_ADR <> f_TEL <> f_EMAIL, warnings)
                        
