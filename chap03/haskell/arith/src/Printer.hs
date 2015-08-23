module Printer where

import Data.Foldable (traverse_)

import Eval
import Types

data Location = Outer | Inner

valOf :: Term a -> String
valOf = show . count 0
  where
    count :: Integer -> Term a -> Integer
    count accum (TermZero _) = accum
    count accum (TermSucc _ innerTerm) = count (succ accum) innerTerm
    count _ _ = error "valOf was passed something other than 0 or succ"

surroundWithParen :: String -> String
surroundWithParen string = "( " ++ string ++ " )"

surroundPrintTerm :: Term a -> String
surroundPrintTerm = surroundWithParen . prettyPrintInner

prettyPrintInner :: Term a -> String
prettyPrintInner (TermTrue _) = "true"
prettyPrintInner (TermFalse _) = "false"
prettyPrintInner (TermZero _) = "0"
prettyPrintInner (TermIf _ innerTerm1 innerTerm2 innerTerm3) =
    "if " ++ surroundPrintTerm innerTerm1 ++
        " then " ++ surroundPrintTerm innerTerm2 ++
        " else " ++ surroundPrintTerm innerTerm3
prettyPrintInner term@(TermSucc _ _)
    | isNumericVal term == True = valOf term
prettyPrintInner (TermSucc _ innerTerm) = "succ " ++ surroundPrintTerm innerTerm
prettyPrintInner (TermPred _ innerTerm) = "pred " ++ surroundPrintTerm innerTerm
prettyPrintInner (TermIsZero _ innerTerm) = "iszero " ++ surroundPrintTerm innerTerm

prettyPrintOutter :: Term a -> String
prettyPrintOutter term@(TermTrue _) = prettyPrintInner term
prettyPrintOutter term@(TermFalse _) = prettyPrintInner term
prettyPrintOutter term@(TermZero _) = prettyPrintInner term
prettyPrintOutter term@(TermSucc _ _)
    | isNumericVal term == True = prettyPrintInner term
prettyPrintOutter term = surroundPrintTerm term

prettyPrintCommand :: Command a -> IO ()
prettyPrintCommand (Eval _ term) = do
    putStr $ prettyPrintOutter term
    putStrLn ";"

prettyPrintCommands :: [Command a] -> IO ()
prettyPrintCommands = traverse_ prettyPrintCommand
