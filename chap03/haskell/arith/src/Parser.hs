{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Parser where

import Control.Monad (void)
import Text.Parsec (
    (<|>), Parsec, ParsecT, SourcePos, Stream, anyChar, char, choice, eof,
    getPosition, many, manyTill, spaces, string, try,
    )

import Types

type MyParser a = forall s m . (Monad m, Stream s m Char) => ParsecT s () m a

parser :: MyParser [Maybe (Command SourcePos)]
parser = do
    spaces
    commands <- many commentOrCommand
    spaces
    return commands
  where
    commentOrCommand :: MyParser (Maybe (Command SourcePos))
    commentOrCommand = try comment <|> command

comment :: MyParser (Maybe (Command SourcePos))
comment = do
    void $ string "/*"
    void $ manyTill anyChar (try (string "*/"))
    spaces
    return Nothing

command :: MyParser (Maybe (Command SourcePos))
command = do
    pos <- getPosition
    term' <- term
    spaces
    void $ char ';'
    spaces
    return . Just $ Eval pos term'

term :: MyParser (Term SourcePos)
term = choice [ trueTerm
              , falseTerm
              ]

trueTerm :: MyParser (Term SourcePos)
trueTerm = do
    pos <- getPosition
    void $ string "true"
    return $ TermTrue pos

falseTerm :: MyParser (Term SourcePos)
falseTerm = do
    pos <- getPosition
    void $ string "false"
    return $ TermFalse pos
