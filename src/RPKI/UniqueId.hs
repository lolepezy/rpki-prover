{-# LANGUAGE OverloadedStrings #-}

module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
<<<<<<< HEAD
<<<<<<< HEAD
    "srcHash#9b83137f4b6a8d97c1e4991e8e3b7e047f5cd0c88feef14ca6bfe2cb21ebe635#srcHash"

=======
    "srcHash#1b663fb9aa22fb8c7bde408c5c6e321b7179f02f4e4682e59e1d5657f615b160#srcHash"
>>>>>>> 135abe7e (Small refactorings)
=======
    "srcHash#40fa4d47d79cb2f7b62eb574b0a34fe7bf8e14a87ede82df1e06710b84133f34#srcHash"
>>>>>>> 7e6a06cc (More comments)
    