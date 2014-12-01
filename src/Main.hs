module Main where

import Control.Monad (forM_)
import Types
import Inference
import Inference.Types
import Unification

program :: [CExp] -> CExp
program = foldr Compose Empty

testUnifier :: CExp -> Maybe Type
testUnifier x = inferType x >>= uncurry unify

testPrograms :: [CExp]
testPrograms = map program [ []
                           , [Dup]
                           , [Dup, Dup]
                           , replicate 10 Dup
                           , [Dup,I]
                           , [I,Dup]
                           ]

main :: IO ()
main = do
  putStrLn "--- full inference test ---"
  forM_ testPrograms $ \e -> do
    let t = testUnifier e
    putStrLn . unwords $ [show e, ":", show t]