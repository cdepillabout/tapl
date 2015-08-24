module Lib where

import Text.Parsec (SourcePos, parse, runParserT)

import Eval
import Options
import Parser (parser)
import Parser2 (parser2)
import Printer
import Types

doCommands :: Eq a => [Command a] -> IO ()
doCommands commands = do
    let evaledCommands = evalCommands commands
    putStrLn "Evaluated:"
    -- print evaledCommands
    prettyPrintCommands evaledCommands

defaultMainReal :: IO ()
defaultMainReal = do
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

defaultMain :: IO ()
-- defaultMain = defaultMainReal
defaultMain = defaultMainTest

defaultMainTest :: IO ()
defaultMainTest = do
    filePath <- getFile
    fileContents <- readFile filePath
    putStrLn "Input file: "
    putStrLn fileContents
    putStrLn ""
    eitherCommands <- runParserT parser2 () filePath fileContents
    putStrLn "Parsed: "
    case eitherCommands of
        Right commands -> do
            print commands
            putStrLn ""
        Left err -> print err
