{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Async

import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad
import qualified Data.List as L

import RPKI.Domain
import RPKI.Parse.Cert
import RPKI.Parse.MFT
import RPKI.Parse.ROA

import System.FilePath.Find

testAllObjects1 = do  
  let repository = "/tmp/traverse-test"  

  let expr = (fileName ~~? "*.cer") ||? 
             (fileName ~~? "*.roa") ||? 
             (fileName ~~? "*.mft") -- add (fileName ~~? "*.crl")
  objs   <- find always expr repository
  parsedObjects <- forM objs $ \file -> do    
              content <- B.readFile file
              let ext = L.drop (L.length file - 3) file
              let parsed = case ext of
                    "cer" -> 
                        case parseResourceCertificate content of
                          Left e  -> do
                            putStrLn $ file ++ ", error = " ++ show e
                            pure $ Left e
                          Right f -> pure $ Right $ show (f (URI (T.pack file)))

                    "mft" -> 
                        case parseMft content of
                          Left e  -> do
                            putStrLn $ file ++ ", error = " ++ show e
                            pure $ Left e
                          Right c -> pure $ Right $ show c

                    "roa" -> 
                      case parseRoa content of
                        Left e  -> do
                          putStrLn $ file ++ ", error = " ++ show e
                          pure $ Left e
                        Right c -> pure $ Right $ show c
                    
                    _     -> pure $ Right ""

              (file, ) <$> parsed
              -- pure (file, Right "")

  let errors = [ (e, p) | (p, Left e) <- parsedObjects ]  
  let objects = [ o | (_, Right o) <- parsedObjects ]

  putStrLn $ "errors = " ++ show (length errors)
  putStrLn $ "n = " ++ show (length parsedObjects)
  putStrLn $ "size = " ++ show (length $ concatMap show objects)
  



main :: IO ()
main = do 
  testAllObjects1
