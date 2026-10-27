-- | What the build knows about itself, worked out at compile time: the hash
-- of the source tree and the git metadata of the checkout.
--
-- The modules that splice these depend on every file they look at, so GHC
-- re-runs the splices whenever one of those files changes. GHC only gets to
-- look when cabal rebuilds the library, though. rpki-prover.cabal lists the
-- files of the executable in extra-source-files to make a change in them do
-- that, but git is not a file it knows about: after a commit with no source
-- change, an incremental build keeps the git metadata of the earlier commit.
module RPKI.Meta.BuildInfo (
    GitInfo(..),
    sourceTreeHash,
    gitInfoOfTheTree
)
where

import           Control.Exception          (IOException, try)
import           Control.Monad

import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as Hex
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Char                  (isSpace)
import           Data.FileEmbed             (makeRelativeToProject)
import           Data.List                  (dropWhileEnd, isPrefixOf, sort)
import           Data.Maybe                 (fromMaybe)
import           Data.String.Interpolate.IsString

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Lift, addDependentFile, lift)

import           System.Directory
import           System.Exit                (ExitCode (..))
import           System.FilePath
import           System.Process.Typed       (proc, readProcess, setWorkingDir)
import           Text.Read                  (readMaybe)


data GitInfo = GitInfo {
        branch      :: String,
        commitHash  :: String,
        commitDate  :: String,
        commitCount :: Int,
        dirty       :: Bool
    }
    deriving stock (Show, Lift)


-- | SHA-256 of the sources and of the configuration they are built with, as a
-- string literal. Two builds with the same one are taken to be the same
-- program. Blank lines and comment lines don't count, and neither does where
-- the tree is.
sourceTreeHash :: Q Exp
sourceTreeHash = do
    root  <- projectRoot
    files <- dependOnSources root
    digest <- runIO $ do
        chunks <- forM files $ \file -> do
            content <- BS.readFile (root </> file)
            pure $ C8.pack file : concatMap (\l -> [l, "\n"]) (significantLines file content)
        pure $ SHA256.finalize $ SHA256.updates SHA256.init $ concat chunks
    litE $ stringL $ C8.unpack $ Hex.encode digest


-- | GitInfo of the checkout, from git when the project is the root of a git
-- repository. Otherwise from build-info/git.txt, which write-build-info.sh
-- writes for builds that can't ask git: cabal install builds an sdist copy
-- somewhere in /tmp, a docker build context has no .git, and git in a
-- container may refuse a repository owned by someone else. "unknown" when
-- neither can tell.
gitInfoOfTheTree :: Q Exp
gitInfoOfTheTree = do
    root <- projectRoot
    let buildInfoFile = root </> "build-info" </> "git.txt"
    haveFile <- runIO $ doesFileExist buildInfoFile
    -- The sources for the dirty flag; HEAD, its log and the index for
    -- commits, checkouts and staging.
    void $ dependOnSources root
    runIO (gitFiles root) >>= mapM_ addDependentFile
    when haveFile $ addDependentFile buildInfoFile
    fromGit <- runIO $ askGit root
    info <- case fromGit of
        Just info -> pure info
        Nothing
            | haveFile -> do
                content <- runIO $ C8.unpack <$> BS.readFile buildInfoFile
                either (fail . (<> " in " <> buildInfoFile)) pure $ parseBuildInfo content
            | otherwise ->
                pure $ GitInfo "unknown" "unknown" "unknown" 0 False
    lift info


projectRoot :: Q FilePath
projectRoot = takeDirectory <$> makeRelativeToProject "rpki-prover.cabal"

-- | The files of the build, relative to the project root, sorted. The module
-- calling it recompiles when any of them changes.
dependOnSources :: FilePath -> Q [FilePath]
dependOnSources root = do
    files <- runIO $ do
        haskell <- concat <$> mapM (filesUnder [".hs"]) ["src", "app"]
        c       <- filesUnder [".c", ".h"] "cbits"
        project <- filterM (doesFileExist . (root </>))
                        ["rpki-prover.cabal", "cabal.project", "cabal.project.freeze"]
        pure $ sort $ project <> haskell <> c
    mapM_ (addDependentFile . (root </>)) files
    pure files
  where
    filesUnder extensions dir = do
        -- Skipping hidden entries skips editor leftovers, like the dangling
        -- symlinks emacs uses as locks.
        entries <- filter (not . ("." `isPrefixOf`)) <$> listDirectory (root </> dir)
        fmap concat $ forM entries $ \entry -> do
            let path = dir </> entry
            isDirectory <- doesDirectoryExist (root </> path)
            if | isDirectory                            -> filesUnder extensions path
               | takeExtension entry `elem` extensions -> pure [path]
               | otherwise                              -> pure []

-- | The lines that count: not blank, and not a "--" comment, except in C,
-- where those aren't comments.
significantLines :: FilePath -> BS.ByteString -> [BS.ByteString]
significantLines file = filter (not . insignificant . C8.dropWhile isSpace) . C8.lines
  where
    insignificant l = BS.null l || (dashComments && "--" `BS.isPrefixOf` l)
    dashComments = takeExtension file `notElem` [".c", ".h"]


-- | Same commands as write-build-info.sh. Nothing unless the project is the
-- top of the repository: an sdist unpacked inside some other repository
-- would get that one's metadata.
askGit :: FilePath -> IO (Maybe GitInfo)
askGit root = do
    topLevel <- git root ["rev-parse", "--show-toplevel"]
    ours <- case topLevel of
        Nothing  -> pure False
        Just top -> (==) <$> canonicalizePath top <*> canonicalizePath root
    if not ours
        then pure Nothing
        else do
            branch      <- orUnknown <$> git root ["rev-parse", "--abbrev-ref", "HEAD"]
            commitHash  <- orUnknown <$> git root ["rev-parse", "HEAD"]
            commitDate  <- orUnknown <$> git root ["log", "-1", "--format=%ci"]
            commitCount <- fromMaybe 0 . (>>= readMaybe) <$> git root ["rev-list", "--count", "HEAD"]
            -- status would otherwise refresh the index, which this module
            -- depends on, and so make the next build recompile it again.
            dirty      <- maybe False (not . null) <$> git root ["--no-optional-locks", "status", "--porcelain"]
            pure $ Just GitInfo {..}
  where
    orUnknown = fromMaybe "unknown"

gitFiles :: FilePath -> IO [FilePath]
gitFiles root =
    git root ["rev-parse", "--absolute-git-dir"] >>= \case
        Nothing     -> pure []
        Just gitDir -> filterM doesFileExist
                            [gitDir </> "HEAD", gitDir </> "logs" </> "HEAD", gitDir </> "index"]

-- | Nothing when git isn't there, or fails.
git :: FilePath -> [String] -> IO (Maybe String)
git root args =
    try (readProcess $ setWorkingDir root $ proc "git" args) >>= \case
        Right (ExitSuccess, out, _) -> pure $ Just $ dropWhileEnd isSpace $ LC8.unpack out
        Right _                     -> pure Nothing
        Left (_ :: IOException)     -> pure Nothing


-- | The key=value lines write-build-info.sh writes.
parseBuildInfo :: String -> Either String GitInfo
parseBuildInfo content = do
    branch      <- field "branch"
    commitHash  <- field "hash"
    commitDate  <- field "date"
    commitCount <- field "count" >>= readField "count"
    dirty       <- field "dirty" >>= readField "dirty"
    pure GitInfo {..}
  where
    fields = [ (key, value) | (key, '=' : value) <- map (break (== '=')) (lines content) ]
    field key = maybe (Left [i|No #{key}|]) Right $ lookup key fields
    readField :: Read a => String -> String -> Either String a
    readField key value = maybe (Left [i|Unreadable #{key} "#{value}"|]) Right $ readMaybe value
