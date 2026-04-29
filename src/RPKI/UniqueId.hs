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
<<<<<<< HEAD
    "srcHash#9b83137f4b6a8d97c1e4991e8e3b7e047f5cd0c88feef14ca6bfe2cb21ebe635#srcHash"
=======
    "srcHash#295ffad8409cbb114135b4f32cc1540e0f8887ca944f9afa2f9ab3ece959a62f#srcHash"
=======
    "srcHash#cebefca946340465d1cddd6da07de40985e0b584b7890acd3315d53d3ea10e4e#srcHash"
>>>>>>> 74eb5433 (Adjust Id)
=======
    "srcHash#d97f8caa75a28483ac5eab0f7583f3c193c53ca2c59feac127f424f5aa8a462b#srcHash"
>>>>>>> f187fb2e (Cleanup)
    
>>>>>>> 646294a8 (Save updates into update log)
