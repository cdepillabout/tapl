module Lib where

import Options

defaultMain :: IO ()
defaultMain = do
    filePath <- getFile
    print filePath
