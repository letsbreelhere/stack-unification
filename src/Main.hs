module Main where

import Control.Monad (forM_)
import Types
import Inference
import Inference.Types
import Unification

program :: [CExp] -> CExp
program [] = Empty
program [x] = x
program (x:xs) = x `Compose` program xs

testUnifier :: CExp -> Either String Type
testUnifier x = inferType x >>= uncurry unify

testPrograms :: [CExp]
testPrograms = map program [ []
                           , [dup_]
                           , [dup_, dup_]
                           , replicate 10 dup_
                           , [i_,dup_]
                           , [Quote dup_]
                           , [Quote dup_, i_, dup_]
                           , [Quote $ Quote $ Quote $ dup_]
                           , [Int 10, Quote i_, dup_]
                           , [Int 10, Bool True, dup_, dup_]
                           -- Uninferencable expressions
                           , [dup_,i_]
                           , [Int 10, i_]
                           ]
  where dup_ = Term "dup"
        i_ = Term "i"

main :: IO ()
main = do
  putStrLn "--- full inference test ---"
  forM_ testPrograms $ \e -> do
    let t = testUnifier e
    putStrLn . unwords $ [show e, ":", show t]
