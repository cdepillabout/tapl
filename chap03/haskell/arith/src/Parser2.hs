{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Parser2 (parser2) where

import Prelude hiding (succ, pred)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (catMaybes)
import Text.Parsec (
    (<|>), Parsec, ParsecT, SourcePos, Stream, alphaNum, anyChar, between,
    char, choice, eof, getPosition, letter, many, manyTill, oneOf,
    optional, spaces, string, try,
    )
import qualified Text.Parsec.Token as Token

import Types

-------------------------
-- parser type synonym --
-------------------------

type MyParser a = forall s u m . (MonadIO m, Monad m, Stream s m Char) => ParsecT s u m a

-------------------------
-- language definition --
-------------------------

langDef :: (Monad m, Stream s m Char) => Token.GenLanguageDef s u m
langDef = Token.LanguageDef
            { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = ""
            , Token.nestedComments  = True
            -- , Token.identStart      = letter <|> char '_'
            , Token.identStart      = alphaNum <|> char '_'
            , Token.identLetter     = alphaNum <|> oneOf "_'"
            , Token.opStart         = Token.opLetter langDef
            , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
            , Token.reservedOpNames = []
            , Token.reservedNames   = {- [ "0"
                                      , "true"
                                      , "false"
                                      , "succ"
                                      , "pred"
                                      , "iszero"
                                      ] -} []
            , Token.caseSensitive   = True
            }

lexer :: (Monad m, Stream s m Char) => Token.GenTokenParser s u m
lexer = Token.makeTokenParser langDef

--------------------------------------
-- parsec language helper functions --
--------------------------------------

identifier :: MyParser String
identifier = Token.identifier lexer

reserved :: String -> MyParser ()
reserved = Token.reserved lexer

lexeme :: MyParser a -> MyParser a
lexeme = Token.lexeme lexer

symbol :: String -> MyParser String
symbol = Token.symbol lexer

semi :: MyParser String
semi = Token.semi lexer

parens :: MyParser a -> MyParser a
parens = Token.parens lexer

whiteSpace :: MyParser ()
whiteSpace = Token.whiteSpace lexer

-------------------------
-- my helper functions --
-------------------------

myIdent :: String -> MyParser ()
myIdent string = do
    ident <- identifier
    if ident == string
        then return ()
        else fail $ "Expecting " ++ string

noArgs :: String -> MyParser ()
noArgs = myIdent

oneArg :: String -> (Term () -> Term ()) -> MyParser (Term ())
oneArg string termConstructor = do
    noArgs string
    t <- term
    return $ termConstructor t

threeArgs :: String
          -> String
          -> String
          -> (Term () -> Term () -> Term () -> Term ())
          -> MyParser (Term ())
threeArgs string1 string2 string3 termConstructor = do
    noArgs string1
    t1 <- term
    noArgs string2
    t2 <- term
    noArgs string3
    t3 <- term
    return $ termConstructor t1 t2 t3

---------------
-- my parser --
---------------

zero :: MyParser (Term ())
zero = noArgs "0" *> return (TermZero ())

true :: MyParser (Term ())
true = noArgs "true" *> return (TermTrue ())

false :: MyParser (Term ())
false = noArgs "false" *> return (TermFalse ())

succ :: MyParser (Term ())
succ = oneArg "succ" $ TermSucc ()

pred :: MyParser (Term ())
pred = oneArg "pred" $ TermPred ()

iszero :: MyParser (Term ())
iszero = oneArg "iszero" $ TermIsZero ()

ifthenelse :: MyParser (Term ())
ifthenelse = threeArgs "if" "then" "else" $ TermIf ()

addSourcePos :: (SourcePos -> MyParser (Term SourcePos)) -> MyParser (Term SourcePos)
addSourcePos f = f =<< getPosition

term :: MyParser (Term ())
term =
    choice $
        fmap try [ parens term
                 , ifthenelse
                 , succ
                 , pred
                 , iszero
                 , zero
                 , true
                 , false
                 ]

command :: MyParser (Command ())
command = do
    t <- term
    semi
    return $ Eval () t

parser2 :: MyParser [Command ()]
parser2 = do
    whiteSpace
    commands <- many command
    eof
    return commands

