{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent.Async
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)

import Data.Bifunctor
import Data.Hex

import RPKI.Domain
import RPKI.Validate
import RPKI.Parse.Common
import RPKI.Parse.Cert
import RPKI.Parse.MFT
import RPKI.Parse.ROA

import System.FilePath.Find

testAllObjects1 = do  
  parsedObjects <- readRepo              

  let errors = [ e | Left e <- parsedObjects ]  
  let objects = [ o | Right os <- parsedObjects, o <- os ]

  let akis = S.fromList [ hex a | Right ros <- parsedObjects, 
                              RpkiObject (RpkiMeta { aki = Just (AKI (KI a)) }) _ <- ros ]

  let skis = S.fromList [ hex a | Right ros <- parsedObjects, 
                              RpkiObject (RpkiMeta { ski = SKI (KI a) }) _ <- ros ]

  putStrLn $ "skis = " ++ show (length skis)
  forM_ skis $ \ski -> putStrLn $ show ski

  putStrLn $ "akis = " ++ show (length akis)
  forM_ akis $ \aki -> putStrLn $ show aki

  putStrLn $ "errors = " ++ show (length errors)
  putStrLn $ "n = " ++ show (length objects)  
  putStrLn $ "validateKeys = " ++ show (validateKeys objects)
  putStrLn $ "map size = " ++ show (MultiMap.size $ byAKIMap objects)
  

readRepo :: IO [Either (ParseError T.Text) [RpkiObject]]
readRepo = do
  let repository = "/Users/mpuzanov/ripe/tmp/rpki/repo"  
  let expr = (fileName ~~? "*.cer") ||? 
             (fileName ~~? "*.roa") ||? 
             (fileName ~~? "*.mft") -- add (fileName ~~? "*.crl")
  objs   <- find always expr repository
  forM objs $ \file -> do    
              content <- B.readFile file
              let ext = L.drop (L.length file - 3) file
              let uri = URI $ T.pack file
              pure $ case ext of
                    "cer" -> do
                        f <- parseResourceCertificate content
                        let (meta, o) = f uri
                        pure $! [RpkiObject meta $ CerRO o]

                    "mft" -> do
                        f <- parseMft content
                        let (meta, o) = f uri
                        pure $! [RpkiObject meta $ MftRO o]

                    "roa" -> do
                        f <- parseRoa content
                        pure $! map (\(meta, o) -> RpkiObject meta $ RoaRO o) $ f uri                        

                    _     -> pure $! [] 

validateKeys objects = 
  case [ ro | ro@(RpkiObject (RpkiMeta { aki = Nothing }) _) <- objects ] of
    []                     -> Left $ "No TA certificate"    
    [RpkiObject meta (CerRO cert)] -> Right $ validateChildren meta cert
  where    
    validateChildren meta cert = map (\(RpkiObject _ c) -> validateSignature c cert) children
      where
        children = MultiMap.lookup (AKI ki) akiMap
        SKI ki = ski meta

    skiMap = bySKIMap objects
    akiMap = byAKIMap objects
    


bySKIMap :: [RpkiObject] -> MultiMap SKI RpkiObject
bySKIMap ros = MultiMap.fromList [ (ski, ro) | ro@(RpkiObject (RpkiMeta {..}) _) <- ros ]

byAKIMap :: [RpkiObject] -> MultiMap AKI RpkiObject
byAKIMap ros = MultiMap.fromList [ (a, ro) | ro@(RpkiObject (RpkiMeta { aki = Just a }) _) <- ros ]


main :: IO ()
main = do 
  testAllObjects1
