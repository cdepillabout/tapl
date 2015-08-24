{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Parser2 (parser2) where

import Prelude hiding (succ, pred)
import Control.Monad.IO.Class (MonadIO)
import Text.Parsec (
    (<|>), ParsecT, SourcePos, Stream, alphaNum, char, choice, eof,
    getPosition, many, oneOf, try,
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
oneArg string termConstructor = fmap termConstructor $ noArgs string *> term

threeArgs :: String
          -> String
          -> String
          -> (Term () -> Term () -> Term () -> Term ())
          -> MyParser (Term ())
threeArgs string1 string2 string3 termConstructor =
    termConstructor <$> (noArgs string1 *> term)
                    <*> (noArgs string2 *> term)
                    <*> (noArgs string3 *> term)

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
term = choice $ fmap try [ parens term
                         , ifthenelse
                         , succ
                         , pred
                         , iszero
                         , zero
                         , true
                         , false
                         ]

command :: MyParser (Command ())
command = Eval () <$> term <* semi

parser2 :: MyParser [Command ()]
parser2 = whiteSpace *> many command <* eof

