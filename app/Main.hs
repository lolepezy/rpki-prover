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

import           System.Directory                 (getDirectoryContents, doesDirectoryExist)
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
import           RPKI.Store.Base.LMDB hiding (getEnv)
import           RPKI.Store.Database
import           RPKI.Store.Util
import           RPKI.TAL
import           RPKI.TopDown
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.Version


type AppEnv = AppContext LmdbStorage


main :: IO ()
main = do
    -- load config file and apply command line options    
    log <- createLogger

    (appContext, validations) <- runValidatorT (vContext $ URI "configuration") $ createAppContext log
    case appContext of
        Left e ->
            logError_ log [i|Couldn't initialise: #{e}|]
        Right appContext' ->
            void $ concurrently
                (runHttpApi appContext')
                (runValidatorApp appContext')


runValidatorApp :: AppEnv -> IO ()
runValidatorApp appContext = do
    -- let tals = [ "afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal" ]
    -- let tals = [ "ripe.tal" ]

    void $ updateWorldVerion $ dynamicState appContext
    result <- bootstapAllTAs appContext
    -- putStrLn $ "done: " <> show result
    pure ()


bootstapAllTAs :: AppEnv -> IO [(Either AppError (), Validations)]
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



runHttpApi :: AppEnv -> IO ()
runHttpApi appContext = Warp.run 9999 $ httpApi appContext


createAppContext :: AppLogger -> ValidatorT vc IO AppEnv
createAppContext log' = do

    -- TODO Make it configuratble?
    let rootDir = ".rpki"        

    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir 
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    lmdb   <- fromEitherM $ first (InitE . InitError) <$> lmdbDir  rootDir 

    lmdbEnv   <- fromTry (InitE . InitError . fmtEx) $ mkLmdb lmdb 1000
    database' <- fromTry (InitE . InitError . fmtEx) $ createDatabase lmdbEnv

    state  <- liftIO createDynamicState

    pure $ AppContext {        
        logger = log',
        config = Config {
            parallelism = getParallelism,
            rsyncConf = RsyncConf rsyncd,
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
          


listTALFiles :: IO [FilePath]
listTALFiles = do 
    home <- getEnv "HOME"
    let talDirectory = home </> ".rpki" </> "tals"
    names <- getDirectoryContents talDirectory
    pure $ map (talDirectory </>) $ 
            filter (".tal" `List.isSuffixOf`) $ 
            filter (`notElem` [".", ".."]) names


talsDir, rsyncDir, tmpDir, lmdbDir :: FilePath -> IO (Either Text FilePath)
talsDir root  = checkSubDirectory root "tals"
rsyncDir root = checkSubDirectory root "rsync"
tmpDir root   = checkSubDirectory root "tmp"
lmdbDir root  = checkSubDirectory root "cache"

checkSubDirectory :: FilePath -> FilePath -> IO (Either Text FilePath)
checkSubDirectory root sub = do
    let talDirectory = root </> sub
    doesDirectoryExist talDirectory >>= \case
        False -> pure $ Left [i| Directory #{talDirectory} doesn't exist.|]
        True ->  pure $ Right talDirectory


data Options = Options {
    rpkiRootDirectory :: FilePath,
    reset :: Bool
} deriving (Eq, Ord, Show, Generic)

