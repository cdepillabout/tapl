{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Parser where

import Text.Parsec (
    (<|>), Parsec, ParsecT, SourcePos, Stream, anyChar, char, eof,
    getPosition, many, manyTill, spaces, string, try,
    )

import Types

type MyParser a = forall s m . (Monad m, Stream s m Char) => ParsecT s () m a

parser :: MyParser [Command SourcePos]
parser = do
    spaces
    commands <- many $ (try comment *> command <* char ';')
    eof
    return commands

comment :: MyParser ()
comment = do
    string "/*"
    manyTill anyChar (try (string "*/"))
    spaces

command :: MyParser (Command SourcePos)
command = do
    com <- Eval <$> getPosition <*> term
    spaces
    return com

term :: MyParser (Term SourcePos)
term = undefined


