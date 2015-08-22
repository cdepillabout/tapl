module Eval where

import Types

isNumericVal :: Term a -> Bool
isNumericVal (TermZero _) = True
isNumericVal (TermSucc _ subTerm) = isNumericVal subTerm
isNumericVal _ = False

findFixedPoint :: (Eq a) => (a -> a) -> a -> a
findFixedPoint f x =
    case f x == x of
        True -> x
        False -> findFixedPoint f $ f x

evalTerm1 :: Term (Maybe a) -> Term (Maybe a)
evalTerm1 (TermIf _ (TermTrue _) subTerm _) = subTerm
evalTerm1 (TermIf _ (TermFalse _) _ subTerm) = subTerm
evalTerm1 (TermIf a condition subTermTrue subTermFalse) =
    let newCond = evalTerm1 condition
    in TermIf a newCond subTermTrue subTermFalse

evalTerm1 (TermSucc a subTerm) =
    let newSubTerm = evalTerm1 subTerm
    in TermSucc a newSubTerm

evalTerm1 (TermPred _ (TermZero _)) = TermZero Nothing
evalTerm1 (TermPred _ (TermSucc _ nv)) | isNumericVal nv = nv
evalTerm1 (TermPred a subTerm) =
    let newSubTerm = evalTerm1 subTerm
    in TermPred a newSubTerm

evalTerm1 (TermIsZero _ (TermZero _)) = TermTrue Nothing
evalTerm1 (TermIsZero _ (TermSucc _ nv)) | isNumericVal nv = TermFalse Nothing
evalTerm1 (TermIsZero a subTerm) =
    let newSubTerm = evalTerm1 subTerm
    in TermIsZero a newSubTerm

evalTerm1 t = t


evalTerm :: (Eq a) => Term (Maybe a) -> Term (Maybe a)
evalTerm term = findFixedPoint evalTerm1 term

evalCommand :: (Eq a) => Command a -> Command (Maybe a)
evalCommand (Eval a term) = Eval (Just a) . evalTerm $ fmap Just term

evalCommands :: (Eq a) => [Command a] -> [Command (Maybe a)]
evalCommands = fmap evalCommand
