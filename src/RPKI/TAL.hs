{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.TAL where

import           Control.DeepSeq
import           Control.Monad
import           Data.Data          (Typeable)
import qualified Data.List          as L
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

import           Codec.Serialise

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Errors

import           RPKI.Util        (convert)


-- | Represents Trust Anchor Locator as described here
-- | https://tools.ietf.org/html/rfc8630
-- | Or as so-called RIPE format
-- | (I couldn't find any formal definiteion of the "RIPE format")
-- | 
data TAL = PropertiesTAL {
    caName              :: Maybe T.Text,
    certificateLocation :: URI,
    publicKeyInfo       :: EncodedBase64,
    prefetchUris        :: [URI]
} | RFC_TAL {
    certificateLocations :: Locations,
    publicKeyInfo        :: EncodedBase64
} deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

certLocations :: TAL -> Locations
certLocations PropertiesTAL {..} = certificateLocation :| []
certLocations RFC_TAL {..}       = certificateLocations

getTaName :: TAL -> TaName
getTaName tal = case tal of 
    PropertiesTAL {..} -> maybe (uri2TaName certificateLocation) TaName caName
    RFC_TAL {..}       -> uri2TaName $ NE.head certificateLocations
    where 
        uri2TaName = \(URI t) -> TaName t

getTaURI :: TAL -> URI
getTaURI PropertiesTAL {..} = certificateLocation
getTaURI RFC_TAL {..} = NE.head certificateLocations

-- | Parse TAL object from raw text
parseTAL :: T.Text -> Either TALError TAL
parseTAL bs = 
    case (parseAsProperties, parseRFC) of
        (Right t, _)       -> Right t
        (Left _,  Right t) -> Right t
        (Left (TALError e1), Left (TALError e2)) -> Left $ TALError $ e1 <> " | " <> e2    
    where
        parseAsProperties = 
            case T.lines bs of 
                [] -> Left $ TALError "Couldn't find newline character."
                lns -> do
                let nonComments = L.filter (\line -> not $ "#" `T.isPrefixOf` line) lns
                properties <- forM nonComments $ \line ->
                    case T.splitOn "=" line of
                    [name, value] -> Right (T.strip name, T.strip value)
                    _             -> Left $ TALError $ convert $ "Line " <> show line <> " doesn't contain '=' sign."
                
                PropertiesTAL <$> 
                    getCaName properties <*>
                    getCertificateLocation properties <*>
                    getPublicKeyInfo properties <*>
                    getPrefetchUris properties      
            where 
                getCaName ps              = Right $ lookup "ca.name" ps
                getCertificateLocation ps = URI     <$> getMandatory "certificate.location" ps
                getPublicKeyInfo       ps = EncodedBase64 . convert <$> getMandatory "public.key.info" ps
                getPrefetchUris ps        = Right $
                    case lookup "prefetch.uris" ps of
                        Nothing          -> []
                        Just prefetchStr -> L.map URI $ T.splitOn "," prefetchStr 

                getMandatory name ps =
                    case lookup name ps of
                        Nothing -> Left $ TALError $ convert $ "'" <> show name <> "' is not defined."
                        Just cl -> Right cl

        parseRFC =      
            case L.span looksLikeUri $ T.lines bs of        
                (_, []) -> Left $ TALError "Empty public key info"
                (uris, base64) ->
                    case NE.nonEmpty uris of
                        Nothing    -> Left $ TALError "Empty list of URIs"
                        Just uris' -> Right $ RFC_TAL {
                                certificateLocations = NE.map (URI . T.strip) uris',
                                publicKeyInfo = EncodedBase64 $ convert $ 
                                    T.concat $ filter (not . T.null) $ map T.strip base64
                            }

        looksLikeUri s = any (`T.isPrefixOf` s) ["rsync://", "http://", "https://"]
