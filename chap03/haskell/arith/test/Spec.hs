{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Comonad (Comonad(..))
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Gen, oneof)
import Test.QuickCheck.Function ((:->), Fun(..), Function(..), functionShow)

import Types

instance Arbitrary a => Arbitrary (Term a) where
    arbitrary :: Gen (Term a)
    arbitrary =
        oneof [ TermTrue <$> arbitrary
              , TermFalse <$> arbitrary
              , TermIf <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
              , TermZero <$> arbitrary
              , TermSucc <$> arbitrary <*> arbitrary
              , TermPred <$> arbitrary <*> arbitrary
              , TermIsZero <$> arbitrary <*> arbitrary
              ]

instance CoArbitrary a => CoArbitrary (Term a)

instance (Read a, Show a) => Function (Term a) where
    function :: (Term a -> b) -> Term a :-> b
    function = functionShow

----------
-- main --
----------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [smallCheckProps, quickCheckProps]

-----------------
-- small check --
-----------------

smallCheckProps :: TestTree
smallCheckProps = testGroup "checked by smallcheck" []

-----------------
-- quick check --
-----------------

extendExtractProp :: (Eq a) => Term a -> Bool
extendExtractProp term = extend extract term == term

extractExtendProp :: (Eq a, Eq b) => Fun (Term a) b -> Term a -> Bool
extractExtendProp (Fun _ f) term = extract (extend f term) == f term

extendExtendProp :: (Eq a, Eq b, Eq c)
                 => Fun (Term a) b
                 -> Fun (Term b) c
                 -> Term a
                 -> Bool
extendExtendProp (Fun _ g) (Fun _ f) term =
    extend f (extend g term) == extend (f . extend g) term

quickCheckProps :: TestTree
quickCheckProps = testGroup "checked by quickcheck"
    [ testProperty
        "extend extract == id"
        (extendExtractProp :: Term Int -> Bool)
    , testProperty
        "extract . extend f == f"
        (extractExtendProp :: Fun (Term Int) String -> Term Int -> Bool)
    , testProperty
        "extend f . extend g == extend (f . extend g)"
        (extendExtendProp :: Fun (Term Int) String
                          -> Fun (Term String) Char
                          -> Term Int
                          -> Bool)
  --  , test2
    ]

-----------
-- hunit --
-----------

unitTests :: TestTree
unitTests = testGroup "unit tests" []
