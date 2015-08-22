{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Parser where

import Control.Monad (void)
import Text.Parsec (
    (<|>), Parsec, ParsecT, SourcePos, Stream, anyChar, between, char,
    choice, eof, getPosition, many, manyTill, optional, spaces, string,
    try,
    )

import Types

type MyParser a = forall s m . (Monad m, Stream s m Char) => ParsecT s () m a

parser :: MyParser [Maybe (Command SourcePos)]
parser = do
    spaces
    commands <- many commentOrCommand
    spaces
    eof
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
term = do
    spaces
    t <- choice [ try $ between (char '(') (char ')') term
                , nonParenTerm
                ]
    spaces
    return t

nonParenTerm :: MyParser (Term SourcePos)
nonParenTerm = do
    spaces
    t <- choice $ fmap try [ trueTerm
                           , falseTerm
                           , zeroTerm
                           , predTerm
                           , succTerm
                           , isZeroTerm
                           , ifThenElseTerm
                           ]
    spaces
    return t

simpleTerm :: (SourcePos -> Term SourcePos)
           -> MyParser a
           -> MyParser (Term SourcePos)
simpleTerm termConstructor termParser = do
    pos <- getPosition
    void termParser
    return $ termConstructor pos

oneArg :: (SourcePos -> Term SourcePos -> Term SourcePos)
       -> MyParser a
       -> MyParser (Term SourcePos)
oneArg termConstructor termParser = do
    pos <- getPosition
    void termParser
    subTerm <- term
    return $ termConstructor pos subTerm

trueTerm :: MyParser (Term SourcePos)
trueTerm = simpleTerm TermTrue $ string "true"

falseTerm :: MyParser (Term SourcePos)
falseTerm = simpleTerm TermFalse $ string "false"

zeroTerm :: MyParser (Term SourcePos)
zeroTerm = simpleTerm TermZero $ string "0"

predTerm :: MyParser (Term SourcePos)
predTerm = oneArg TermPred $ string "pred"

succTerm :: MyParser (Term SourcePos)
succTerm = oneArg TermSucc $ string "succ"

isZeroTerm :: MyParser (Term SourcePos)
isZeroTerm = oneArg TermIsZero $ string "iszero"

ifThenElseTerm :: MyParser (Term SourcePos)
ifThenElseTerm = do
    pos <- getPosition
    void $ string "if"
    subTerm1 <- term
    void $ string "then"
    subTerm2 <- term
    void $ string "else"
    subTerm3 <- term
    return $ TermIf pos subTerm1 subTerm2 subTerm3
