{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}

module RPKI.TAL where

import Data.Bifunctor (first, bimap)

import           Control.Lens                     ((^.))
import           Control.Monad

import qualified Data.List                        as List
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set.NonEmpty                as NESet
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Text.Encoding               (encodeUtf8)
import qualified Data.ByteString                  as BS

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Reporting

import           RPKI.Util                 
import           RPKI.Store.Base.Serialisation       



-- | Represents Trust Anchor Locator as described here
-- | https://tools.ietf.org/html/rfc8630
-- | Or as so-called RIPE format
-- | (I couldn't find any formal definiteion of the "RIPE format")
-- | 
data TAL = PropertiesTAL {
        caName              :: Maybe Text.Text,
        certificateLocation :: Locations,
        publicKeyInfo       :: EncodedBase64,
        prefetchUris        :: [RpkiURL],
        taName              :: TaName
    } 
    | RFC_TAL {
        certificateLocations :: Locations,
        publicKeyInfo        :: EncodedBase64,
        taName               :: TaName
    } 
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary)

talCertLocations :: TAL -> Locations
talCertLocations PropertiesTAL {..} = certificateLocation
talCertLocations RFC_TAL {..}       = certificateLocations

getTaName :: TAL -> TaName
getTaName = (^. #taName)    

getTaCertURL :: TAL -> RpkiURL
getTaCertURL PropertiesTAL {..} = pickLocation certificateLocation
getTaCertURL RFC_TAL {..}       = pickLocation certificateLocations

newLocation :: Text.Text -> NonEmpty RpkiURL
newLocation t =  RrdpU (RrdpURL $ URI t) :| []

-- | Parse TAL object from raw text
parseTAL :: Text.Text -> Text.Text -> Either TALError TAL
parseTAL bs taName = 
    case validTaName taName of 
        Left e -> Left e
        Right taName' -> 
            case (parseAsProperties taName', parseRFC taName') of
                (Right t, _)       -> Right t
                (Left _,  Right t) -> Right t
                (Left (TALError e1), Left (TALError e2)) -> 
                    Left $ TALError $ e1 <> " | " <> e2    
    where
        parseAsProperties taName' = 
            case Text.lines bs of 
                [] -> Left $ TALError "Couldn't find newline character."
                lns -> do     
                    -- map each line to a (property name, property value) pair
                    properties <- forM (nonComments lns) $ \line ->
                        case Text.splitOn "=" line of
                            [name, value] -> Right (Text.strip name, Text.strip value)
                            _             -> Left $ TALError [i|Line #{line} doesn't contain '=' sign. |]
                
                    PropertiesTAL <$> 
                        getCaName properties <*>
                        getCertificateLocation properties <*>
                        getPublicKeyInfo properties <*>
                        getPrefetchUris properties <*>     
                        pure taName'
            where 
                -- Lines that are not comments are the ones not startig with '#'
                nonComments = List.filter $ not . ("#" `Text.isPrefixOf`) . Text.stripStart 

                getCaName ps              = Right $ lookup "ca.name" ps                
                getPublicKeyInfo       ps = EncodedBase64 . convert <$> getMandatory "public.key.info" ps
                getPrefetchUris ps        = first TALError $ 
                    case lookup "prefetch.uris" ps of
                        Nothing          -> Right []
                        Just prefetchStr -> mapM parseRpkiURL 
                                            $ Text.splitOn "," prefetchStr 

                getCertificateLocation ps = 
                    case lookup propertyName ps of
                        Nothing   -> Left  $ TALError [i|'#{propertyName}' is not defined.|]
                        Just uris -> 
                            case NonEmpty.nonEmpty (Text.splitOn "," uris) of
                                Nothing    -> Left $ TALError [i|Empty list of TA certificate URLs in #{propertyName}|]
                                Just uris' -> bimap TALError (Locations . NESet.fromList) $ mapM parseRpkiURL uris'
                    where
                        propertyName = "certificate.location" :: Text.Text

                getMandatory name ps =
                    case lookup name ps of
                        Nothing -> Left $ TALError [i|'#{name}' is not defined.|]
                        Just cl -> Right cl

        parseRFC taName' =      
            case List.span looksLikeUri $ Text.lines bs of        
                (_, [])        -> Left $ TALError "Empty public key info"
                (uris, base64) ->
                    case NonEmpty.nonEmpty uris of
                        Nothing    -> Left $ TALError "Empty list of URIs"
                        Just uris' -> do 
                            locations <- first TALError 
                                            $ mapM parseRpkiURL 
                                            $ NonEmpty.map Text.strip uris'
                            pure $ RFC_TAL {
                                certificateLocations = Locations $ NESet.fromList locations,
                                publicKeyInfo = EncodedBase64 $ convert $ 
                                    Text.concat $ filter (not . Text.null) $ map Text.strip base64,
                                taName = taName'
                            }
            where 
                looksLikeUri s = any (`Text.isPrefixOf` s) ["rsync://", "http://", "https://"]


validTaName :: Text.Text -> Either TALError TaName
validTaName taName = 
    if BS.length (encodeUtf8 taName) > 512
        then 
            Left $ TALError [i|TA name #{taName} is too long, it should be <= 512 bytes.|]
        else
            Right $ TaName taName