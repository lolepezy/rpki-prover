{-# LANGUAGE OverloadedStrings    #-}
module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
<<<<<<< HEAD
    "srcHash#f9716b22896a1e0871a0d396f5687e43c76e2d500ac38ff535f4142fbe7cd8ba#srcHash"
=======
    "srcHash#28d120fa1bedd86733bd920e1065f615a8dd4a963b4dd8f89cade64c77447ddb#srcHash"
>>>>>>> 163f9a3 (Manifest location and SIA mismatch is not an error)
