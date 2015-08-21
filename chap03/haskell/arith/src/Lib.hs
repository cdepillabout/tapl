module Lib where

import Text.Parsec (parse)

import Options
import Parser

defaultMain :: IO ()
defaultMain = do
    filePath <- getFile
    fileContents <- readFile filePath
    print $ parse parser filePath fileContents
