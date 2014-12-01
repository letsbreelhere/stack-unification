module Test where

import Catype

program :: [CExp] -> CExp
program = foldr Compose Empty

testUnifier' :: CExp -> Maybe Type
testUnifier' x = do (t,es) <- inferType x
                    unify t es

testUnifier :: [CExp] -> Maybe Type
testUnifier = testUnifier' . program

main :: IO ()
main = do
  putStr "\n"
  putStrLn "--- full inference test ---"
  mapM_ print [ testUnifier []
              , testUnifier [Dup]
              , testUnifier [Dup, Dup]
              , testUnifier $ replicate 10 Dup
              , testUnifier [Dup, I]
              , testUnifier [I, Dup]
              ]
