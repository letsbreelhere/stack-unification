module Main where

import Control.Monad ((>=>))
import Data.Foldable (forM_)
import Types
import Inference
import Unification
import Parser

testUnifier :: CExp -> Either String Type
testUnifier = inferType >=> uncurry unify

testPrograms :: [String]
testPrograms = [ ""
               , "dup"
               , "dup dup"
               , unwords $ replicate 10 "dup"
               , "i dup"
               , "[dup]"
               , "[dup] i dup"
               , "[[[dup]]]"
               , "10 #t dup dup"
               -- Uninferencable expressions
               , "dup i"
               , "10 i"
               -- Unparseable expressions
               , "000nope000"
               ]

main :: IO ()
main = forM_ testPrograms $ \s -> do
  let mParsed = parseExpression s
      t = either (Left . show) testUnifier mParsed
  putStrLn . unwords $ [s, ":", show t]
