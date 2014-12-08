module Main where

import Control.Applicative
import Data.Monoid
import Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad ((>=>))
import Data.Either (isLeft, isRight)
import Data.Foldable (fold)

import Inference
import Parser
import Types
import Unification

testUnifier :: CExp -> Either String Type
testUnifier = inferType >=> uncurry unify

testRight :: (Show a, Show b) => TestName -> Either a b -> TF.Test
testRight str e = testCase str . assertBool ("expected " ++ show e ++ " to be Right") $ isRight e


testLeft :: (Show a, Show b) => TestName -> Either a b -> TF.Test
testLeft str e = testCase str . assertBool ("expected " ++ show e ++ " to be Left") $ isLeft e

parserTests :: [TF.Test]
parserTests =
  [ testCase "empty program" . assertBool "" $ either (const False) id $ (== Empty) <$> parseExpression ""
  , testRight "comments" $ parseExpression "a {{ block comment! }} b"
  , testLeft "unparseable" $ parseExpression "#wrong"
  ]

inferenceTests :: [TF.Test]
inferenceTests =
  [ testRight "empty program" $ testUnifier Empty
  , testRight "dup" $ testUnifier dup_
  , testRight "dup dup" $ testUnifier (dup_ `Compose` dup_)
  , testRight "dup dup dup dup dup dup dup dup dup dup" $ testUnifier (fold $ replicate 10 dup_)
  , testLeft "dup i" $ testUnifier (dup_ `Compose` i_)
  , testLeft "10 i" $ testUnifier (Int 10 `Compose` i_)
  ]
  where dup_ = Term "dup"
        i_ = Term "i"

main :: IO ()
main = defaultMainWithOpts
       (parserTests ++ inferenceTests)
       mempty
