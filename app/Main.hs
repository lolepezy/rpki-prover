{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Colog

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted

import           Data.Bifunctor
import qualified Data.ByteString                  as BS
import qualified Data.List                        as List
import           Data.Text                        (Text)
import qualified Data.Text                        as Text

import           Data.String.Interpolate.IsString

import qualified Network.Wai.Handler.Warp         as Warp

import           System.Directory                 (getDirectoryContents)
import           System.Environment
import           System.FilePath                  ((</>))

import           Options.Generic

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Execution
import           RPKI.Http.Server
import           RPKI.Logging
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Util
import           RPKI.TAL
import           RPKI.TopDown
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.Version



--
servantAPI :: IO ()
servantAPI = pure ()

main :: IO ()
main = do
    -- load config file and apply command line options
    -- initialise environment and LMDB

    lmdbEnv  <- mkLmdb "./data" 1000
    database' <- createDatabase lmdbEnv
    appContext <- createAppContext database'

    void $ concurrently
        (runHttpApi appContext)
        (runValidatorApp appContext)


runValidatorApp :: Storage s => AppContext s -> IO ()
runValidatorApp appContext = do
    let tals = [ "afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal" ]
    -- let tals = [ "ripe.tal" ]

    void $ updateWorldVerion $ dynamicState appContext
    result <- bootstapAllTAs appContext
    -- putStrLn $ "done: " <> show result
    pure ()


bootstapAllTAs :: Storage s => AppContext s -> IO [(Either AppError (), Validations)]
bootstapAllTAs appContext@AppContext {..} = do
    talFileNames <- listTALFiles
    asyncs <- forM talFileNames $ \talFileName -> 
        async $ do
            (talContent, vs) <- runValidatorT (vContext $ URI $ convert talFileName) $ do
                                    t <- fromTry (RsyncE . FileReadError . fmtEx) $ BS.readFile talFileName
                                    vHoist $ fromEither $ first TAL_E $ parseTAL $ convert t    
            case talContent of 
                Left e -> do
                    logError_ logger [i|Error reading TAL #{talFileName}, e = #{e}.|]
                    pure (Left e, vs)
                Right talContent' -> 
                    bootstrapTA appContext talContent'

    forM asyncs wait


listTALFiles :: IO [FilePath]
listTALFiles = do 
    home <- getEnv "HOME"
    let talDirectory = home </> ".rpki" </> "tals"
    names <- getDirectoryContents talDirectory
    pure $ map (talDirectory </>) $ 
            filter (".tal" `List.isSuffixOf`) $ 
            filter (`notElem` [".", ".."]) names


runHttpApi :: Storage s => AppContext s -> IO ()
runHttpApi appContext = Warp.run 9999 $ httpApi appContext


createAppContext :: DB s -> IO (AppContext s)
createAppContext database' = do 
    log'  <- createLogger
    state <- createDynamicState
    pure $ AppContext {        
        logger = log',
        config = Config {
            parallelism = getParallelism,
            rsyncConf = RsyncConf "/tmp/rsync",
            validationConfig = ValidationConfig $ 24 * 3600 
        },
        dynamicState = state,
        database = database'
    }

createLogger :: IO AppLogger
createLogger = do 
    -- TODO Use colog-concurrent instead of this
    lock <- newMVar True
    pure $ AppLogger fullMessageAction lock
    where
        fullMessageAction = upgradeMessageAction defaultFieldMap $ 
            cmapM fmtRichMessageDefault logTextStdout  
          

data Options = Options {

}
