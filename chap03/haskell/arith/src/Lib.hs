module Lib where

import Text.Parsec (SourcePos, parse)

import Eval
import Options
import Parser
import Printer
import Types

doCommands :: [Command SourcePos] -> IO ()
doCommands commands = do
    let evaledCommands = evalCommands commands
    putStrLn "Evaluated:"
    -- print evaledCommands
    prettyPrintCommands evaledCommands

defaultMain :: IO ()
defaultMain = do
    filePath <- getFile
    fileContents <- readFile filePath
    putStrLn "Input file: "
    putStrLn fileContents
    putStrLn ""
    let eitherCommands = parse parser filePath fileContents
    putStrLn "Parsed: "
    case eitherCommands of
        Right commands -> do
            print commands
            putStrLn ""
            doCommands commands
        Left err -> print err

