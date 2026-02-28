{-# LANGUAGE OverloadedStrings #-}

module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
<<<<<<< HEAD
    "srcHash#43fe6363f83616f9fe0fb712203f0c00c02d80c1ba602d550d18e5acd1e39db6#srcHash"
=======
    "srcHash#43fe6363f83616f9fe0fb712203f0c00c02d80c1ba602d550d18e5acd1e39db6#srcHash"
>>>>>>> master
    
