{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types where

import Control.Comonad (Comonad(..))
import GHC.Generics (Generic)

data Term a = TermTrue a
            | TermFalse a
            | TermIf a (Term a) (Term a) (Term a)
            | TermZero a
            | TermSucc a (Term a)
            | TermPred a (Term a)
            | TermIsZero a (Term a)
    deriving (Eq, Functor, Generic, Ord, Read, Show)

data Command a = Eval a (Term a)
    deriving (Eq, Functor, Generic, Ord, Read, Show)

instance Comonad Term where
    extract :: Term a -> a
    extract (TermTrue a) = a
    extract (TermFalse a) = a
    extract (TermZero a) = a
    extract (TermSucc a _) = a
    extract (TermPred a _) = a
    extract (TermIsZero a _) = a
    extract (TermIf a _ _ _) = a

    extend :: (Term a -> b) -> Term a -> Term b
    extend c ta@(TermTrue _) = TermTrue $ c ta
    extend c ta@(TermFalse _) = TermFalse $ c ta
    extend c ta@(TermZero _) = TermZero $ c ta
    extend c ta@(TermSucc _ ta2) = TermSucc (c ta) (extend c ta2)
    extend c ta@(TermPred _ ta2) = TermPred (c ta) (extend c ta2)
    extend c ta@(TermIsZero _ ta2) = TermIsZero (c ta) (extend c ta2)
    extend c ta@(TermIf _ ta2 ta3 ta4) =
        TermIf (c ta) (extend c ta2) (extend c ta3) (extend c ta4)
