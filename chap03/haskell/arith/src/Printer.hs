module Printer where

import Data.Foldable (traverse_)

import Types


prettyPrintTerm :: Term a -> IO ()
prettyPrintTerm term = return ()

prettyPrintCommand :: Command a -> IO ()
prettyPrintCommand (Eval _ term) = do
    prettyPrintTerm term
    putStrLn ";"

prettyPrintCommands :: [Command a] -> IO ()
prettyPrintCommands = traverse_ prettyPrintCommand
