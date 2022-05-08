{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Internal.RSC where

import qualified Data.ByteString as BS  

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.Bifunctor
import Data.String.Interpolate.IsString

import Data.Tuple.Strict

import RPKI.Domain 
import RPKI.Resources.Types
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject 

import RPKI.Util
import RPKI.Resources.Resources as R
import RPKI.Resources.IntervalSet as IS
import Data.Either


-- | Parse RSC, https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/
-- 
parseRSC :: BS.ByteString -> ParseResult RscObject
parseRSC bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs      
    signedRsc <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseRsc') asns
    hash' <- getMetaFromSigned signedRsc bs
    pure $ newCMSObject hash' (CMS signedRsc)
  where     
    parseRsc' = onNextContainer Sequence $ do        
        ((v4, v6), asn) <- parseResourceSet        
        digestAlgorithm <- onNextContainer Sequence getDigest
        checkList       <- parseCheckList
        let rscResources = PrefixesAndAsns v4 v6 asn
        pure RSC {..}

    parseResourceSet = 
        onNextContainer Sequence $ do
            asns <- getNextContainerMaybe (Container Context 0) >>= \case 
                Nothing -> pure IS.empty
                Just asn1  -> 
                    case runParseASN1 parseAsns asn1 of
                        Left e  -> throwParseError $ show e
                        Right z -> pure z                         
            
            ips <- getNextContainerMaybe (Container Context 1) >>= \case 
                    Nothing -> pure (IS.empty, IS.empty)
                    Just asn1  -> 
                        case runParseASN1 parseIps asn1 of
                            Left e  -> throwParseError $ show e
                            Right z -> pure z
                                    
            pure (ips, asns)

    parseCheckList = 
        onNextContainer Sequence $ 
            getMany $ 
                onNextContainer Sequence $ 
                    getNext >>= \case 
                        ASN1String (ASN1CharacterString IA5 filename) ->
                            getNext >>= \case
                                OctetString os -> pure $ T2 (Just $ convert filename) (mkHash os)
                                other          -> throwParseError $ "Unexpected checklist item: " ++ show other
                        OctetString os         -> pure $ T2 Nothing (mkHash os)
                        other                  -> throwParseError $ "Unexpected checklist item: " ++ show other
    
    parseIps = do 
        z <- onNextContainer Sequence $ do                                        
                getMany $ 
                    onNextContainer Sequence $ do                         
                        getAddressFamily "Expected an address family here" >>= \case
                            Right Ipv4F -> Left  <$> ipv4Address
                            Right Ipv6F -> Right <$> ipv6Address
                            Left af     -> throwParseError $ "Unsupported address family " <> show af                
        pure $ bimap 
            (IS.fromList . mconcat) 
            (IS.fromList . mconcat) 
            $ partitionEithers z        

    parseAsns = IS.fromList <$> onNextContainer Sequence (getMany asOrRange)
        
    

{-
RSC:

    Start Sequence,
        Start Sequence,
            Start (Container Context 1),
                Start Sequence,
                    Start Sequence,
                        OctetString \"\\NUL\\STX\",
                        BitString (BitArray 48 \" \\SOH\\ACK| \\140\"),
                    End Sequence,
                End Sequence,
            End (Container Context 1),
        End Sequence,
        Start Sequence,
            OID [2,16,840,1,101,3,4,2,1],
        End Sequence,
        Start Sequence,
            Start Sequence,
                ASN1String (ASN1CharacterString {characterEncoding = IA5, getCharacterStringRawData = \"b42_ipv6_loa.png\"}),
                OctetString \"\\149\\SYN\\221d\\190|\\ETB%\\185\\252\\161\\ETB\\DC2\\SOX\\232\\216B\\165 hs9\\155=\\223\\252\\145\\196\\182\\172\\240\",                
            End Sequence,
            Start Sequence,
                OctetString \"\\n\\225\\&9G\\\"\\NUL\\\\\\217/Lj\\160$\\213\\214\\179\\226\\230}b\\159\\DC1r\\r\\148x\\166\\&3\\161\\ETB\\161\\199\",
            End Sequence,
        End Sequence,
    End Sequence

resources from a cert:

    Start Sequence,
        Start Sequence,
            OctetString "\NUL\SOH",
            Start Sequence,
                BitString (BitArray 0 ""),
            End Sequence,
        End Sequence,
        Start Sequence,
            OctetString "\NUL\STX",
            Start Sequence,
                BitString (BitArray 0 ""),
            End Sequence,
        End Sequence,
    End Sequence

or: 

    Start Sequence,
        Start Sequence,
            OctetString "\NUL\SOH",
            Start Sequence,
                BitString (BitArray 8 "\STX"),
                Start Sequence,
                    BitString (BitArray 8 "\ENQ"),
                    BitString (BitArray 19 "\ENQ\FS\NUL"),
                End Sequence,
                Start Sequence,
                    BitString (BitArray 21 "\ENQ\FS("),
                    BitString (BitArray 7 "\EOT"),
                End Sequence,
                Start Sequence,
                    BitString (BitArray 14 "\rt"),
                    BitString (BitArray 14 "\rx"),
                End Sequence,
                BitString (BitArray 14 "\r\140"),
                Start Sequence,
                    BitString (BitArray 13 "\r\168"),
                    BitString (BitArray 13 "\r\176"),
                End Sequence,
                BitString (BitArray 14 "\r\240"),
                BitString (BitArray 22 "\ETB\DC30"),
                BitString (BitArray 21 "\ETB\DC38"),
                BitString (BitArray 21 "\ETBS@"),
                BitString (BitArray 20 "\ETBSp"),
                BitString (BitArray 19 "\ETBi\224"),
                Start Sequence,
                    BitString (BitArray 21 "\ETBjh"),
                    BitString (BitArray 21 "\ETBjp"),
                End Sequence,
                BitString (BitArray 20 "\ETBl\208"),
                BitString (BitArray 16 "\ETBm"),
                BitString (BitArray 21 "\ETBo\NUL"),
                .....            
-}



{-
certificate: 

Start Sequence,
    Start Sequence,
        Start (Container Context 0),
            IntVal 2,
        End (Container Context 0),
        IntVal 212,
        Start Sequence,
            OID [1,2,840,113549,1,1,11],
            Null,
        End Sequence,
        Start Sequence,
            Start Set,
                Start Sequence,
                    OID [2,5,4,3],
                    ASN1String (ASN1CharacterString {characterEncoding = Printable, getCharacterStringRawData = "ripe-ncc-ta"}),
                End Sequence,
            End Set,
        End Sequence,
        Start Sequence,
            ASN1Time TimeUTC (DateTime {dtDate = Date {dateYear = 2018, dateMonth = December, dateDay = 18}, dtTime = TimeOfDay {todHour = 13h, todMin = 22m, todSec = 11s, todNSec = 0ns}}) (Just +0000),
            ASN1Time TimeUTC (DateTime {dtDate = Date {dateYear = 2019, dateMonth = July, dateDay = 1}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}) (Just +0000),
        End Sequence,
        Start Sequence,
            Start Set,
                Start Sequence,
                    OID [2,5,4,3],
                    ASN1String (ASN1CharacterString {characterEncoding = Printable, getCharacterStringRawData = "2a7dd1d787d793e4c8af56e197d4eed92af6ba13"}),
                End Sequence,
            End Set,
        End Sequence,
        Start Sequence,
            Start Sequence,
                OID [1,2,840,113549,1,1,1],
                Null,
            End Sequence,
            BitString (BitArray 2160 "0\130\SOH\n\STX\130\SOH\SOH\NUL\234\218B\ESCZH\166\&2yx\153\229\238\191\199\187\164\236\226\154\235\172^\DLE\168}\177\220\253\"\180\207\227\230'`\166\249\"\244\ENQ$\250\166T\138\SOHC\195\174\t\ETXC\159\225\210&\201m\DC4(S\248\226\190\"\ETX\229\187#\b\233\SO9yZ\170\252\182$\233\225\152\173\GS\GS\245B\209p\220\209\200M\254\132bi\196\219\251c\221\181\144NOt\160~\ETXv\161\v\r\251\163nYW.\DEL\US^\135\218\145\133b8\b\157=p\241\226O:d\190-9U5p56w\240\b]\ENQ\203\219\b\140.U\130\179\245\164\240\237\RSa7aO\236:\174\246`\SUB\201\159\151'%tR2\205!m\210 \128y\248\DC2\128\a\152\219\140\156?/\160k\v(P\198\166\180+\177W\245_\208?\DEL\227v\US\196\237MB\135Pu\vE\136 \140'\198\244\"\ETX\EOT\179\209\249\165\250j,K%\229\&7\235\185Ci\STX\161\202G\STX\ETX\SOH\NUL\SOH"),
        End Sequence,
        Start (Container Context 3),
            Start Sequence,
                Start Sequence,
                    OID [2,5,29,14],
                    OctetString "\EOT\DC4*}\209\215\135\215\147\228\200\175V\225\151\212\238\217*\246\186\DC3",
                End Sequence,
                Start Sequence,
                    OID [2,5,29,35],
                    OctetString "0\SYN\128\DC4\232U+\US\214\209\164\247\228\EOT\198\216\229h\r\RS\188\SYN?\195",
                End Sequence,
                Start Sequence,
                    OID [2,5,29,19],
                    Boolean True,
                    OctetString "0\ETX\SOH\SOH\255",
                End Sequence,
                Start Sequence,
                    OID [2,5,29,15],
                    Boolean True,
                    OctetString "\ETX\STX\SOH\ACK",
                End Sequence,
                Start Sequence,
                    OID [1,3,6,1,5,5,7,1,1],
                    OctetString "0604\ACK\b+\ACK\SOH\ENQ\ENQ\a0\STX\134(rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer",
                End Sequence,
                Start Sequence,
                    OID [1,3,6,1,5,5,7,1,11],
                    OctetString "0\129\185\&01\ACK\b+\ACK\SOH\ENQ\ENQ\a0\ENQ\134%rsync://rpki.ripe.net/repository/aca/0P\ACK\b+\ACK\SOH\ENQ\ENQ\a0\n\134Drsync://rpki.ripe.net/repository/aca/Kn3R14fXk-TIr1bhl9Tu2Sr2uhM.mft02\ACK\b+\ACK\SOH\ENQ\ENQ\a0\r\134&https://rrdp.ripe.net/notification.xml",
                End Sequence,
                Start Sequence,
                    OID [2,5,29,31],
                    OctetString "0806\160\&4\160\&2\134\&0rsync://rpki.ripe.net/repository/ripe-ncc-ta.crl",
                End Sequence,
                Start Sequence,
                    OID [2,5,29,32],
                    Boolean True,
                    OctetString "0\f0\n\ACK\b+\ACK\SOH\ENQ\ENQ\a\SO\STX",
                End Sequence,
                Start Sequence,
                    OID [1,3,6,1,5,5,7,1,7],
                    Boolean True,
                    OctetString "0\SYN0\t\EOT\STX\NUL\SOH0\ETX\ETX\SOH\NUL0\t\EOT\STX\NUL\STX0\ETX\ETX\SOH\NUL",
                End Sequence,
                Start Sequence,
                    OID [1,3,6,1,5,5,7,1,8],
                    Boolean True,
                    OctetString "0\DLE\160\SO0\f0\n\STX\SOH\NUL\STX\ENQ\NUL\255\255\255\255",
                End Sequence,
            End Sequence,
        End (Container Context 3),
    End Sequence,
    Start Sequence,
        OID [1,2,840,113549,1,1,11],
        Null,
    End Sequence,
    BitString (BitArray 2048 "b\SUBuE\NAK\194}\131\USg\246\133\SI\144\201z\187\ETX\203}Pf\235\&9~\187(\140`\170\211\v\DC1\223P\143\NAK\217n\v\206\ENQ@\237\249\215e\CAN\182\197\195\239\ETX{\155\132\242\136}M\158P%N#\vC4\FS\153\STX\DC2\155jcCD\a(\180\al|\129\148\&5\a/\196\194\n]\210!\227L,M\SUB\177\SOH\"\182\&7\161\&0\142\255\DC3@\200\179\&0u4\161\219\156\240;\140\219\DC4\165~x\239\235\EOTn\ENQ\NAK\131'!\227\nEEP\191\206\223\141U\203\203<Z\192\247E2\130\151\204\145\&5\142\v_\170b\202\CAN\146\250\&6\201\157\128\DC1O^\215\a\232\166\189\200\218\149\227\163\172\a2\188Fg8\252\ACK\160G\228\239\165\158\182\207\144\145\194z\SUB\227VTun}B\132QO\152F\244n\143\247\&7?\155\192Tr\196\132\186\US\254\132\156\159\148\145\248\242\228-,l\218\DC2\139\239\158O%\RSkH\ESC\SI"),
    End Sequence

resources:
    Start Sequence,
        Start Sequence,
            OctetString "\NUL\SOH",
            Start Sequence,
                BitString (BitArray 0 ""),
            End Sequence,
        End Sequence,
        Start Sequence,
            OctetString "\NUL\STX",
            Start Sequence,
                BitString (BitArray 0 ""),
            End Sequence,
        End Sequence,
    End Sequence

-}

-- cContent = RSC {
--     ipResources = IpResources (IpResourceSet (RS []) (RS [])), 
--     asResources = AsResources (RS []), 
--     checkList = [
--         T2 (Just "b42_ipv6_loa.png") "9516dd64be7c1725b9fca117120e58e8d842a5206873399b3ddffc91c4b6acf0",
--         T2 Nothing "0ae1394722005cd92f4c6aa024d5d6b3e2e67d629f11720d9478a633a117a1c7"]
--     }