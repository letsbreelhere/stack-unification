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

testUnifier :: CExp -> Maybe Type
testUnifier x = do (t, es) <- inferType x
                   unify t es

testPrograms :: [CExp]
testPrograms = map program [ []
                           , [Dup]
                           , [Dup, Dup]
                           , replicate 10 Dup
                           , [Dup,I]
                           , [I,Dup]
                           , [Quote $ program [Dup]]
                           , [Quote $ program [Dup], I, Dup]
                           ]

main :: IO ()
main = do
  putStrLn "--- full inference test ---"
  forM_ testPrograms $ \e -> do
    let t = testUnifier e
    putStrLn . unwords $ [show e, ":", show t]
