{-# LANGUAGE OverloadedStrings #-}

-- | One-off migration helper for the `urls.url` TEXT -> BLOB storage change
-- (database version 54 -> 55: see the `urls` table in RPKI.Store.SQLite and
-- RPKI.Store.Database.currentDatabaseVersion). Converts an EXISTING SQLite
-- cache in place, preserving `url_key` values (referenced by object_urls),
-- instead of paying for the normal version-mismatch handling in
-- RPKI.Store.AppSqliteStorage.setupSqliteCache, which just drops and
-- rebuilds the whole cache from scratch on any version bump.
--
-- This is meant for benchmarking against a real, already-populated cache
-- without re-fetching it from the network. A real deployment upgrading
-- past v55 will simply get the normal full rebuild, as it already does
-- for any version bump.
--
-- Usage: cabal run migrate-urls-to-blob -- /path/to/cache/rpki-cache.sqlite
module Main where

import qualified Data.Text        as Text

import           Database.SQLite.Simple

import           System.Environment (getArgs)
import           Text.Printf (printf)

import           RPKI.Store.Base.Storable (serialiseField)
import           RPKI.Util (parseRpkiURL)


main :: IO ()
main = do
    args <- getArgs
    dbPath <- case args of
        [p] -> pure p
        _   -> error "Usage: migrate-urls-to-blob <path/to/rpki-cache.sqlite>"

    conn <- open dbPath

    rows <- query_ conn "SELECT url_key, url FROM urls" :: IO [(Int, Text.Text)]
    printf "Migrating %d urls rows from TEXT to BLOB...\n" (length rows)

    execute_ conn "CREATE TABLE urls_new (url_key INTEGER PRIMARY KEY, url BLOB NOT NULL UNIQUE)"

    withTransaction conn $
        mapM_ (\(urlKey, urlText) ->
            case parseRpkiURL urlText of
                Left e ->
                    error $ "Unparseable URL " <> Text.unpack urlText <> ": " <> Text.unpack e
                Right rpkiUrl ->
                    execute conn "INSERT INTO urls_new(url_key, url) VALUES (?, ?)"
                        (urlKey, serialiseField rpkiUrl))
            rows

    execute_ conn "DROP TABLE urls"
    execute_ conn "ALTER TABLE urls_new RENAME TO urls"
    execute conn "UPDATE metadata SET value = ? WHERE key = 'database-version'" (Only ("55" :: Text.Text))

    close conn
    putStrLn "Migration complete."
