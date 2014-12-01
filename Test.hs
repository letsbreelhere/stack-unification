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
  putStrLn "--- substituteStackInStack ---"
  let k = 1
      s = [TVar 0] :# 0
      t = [TVar 2] :# 1
      t' = [TVar 2, TVar 0] :# 0

  putStrLn "--- varsOf ---"
  mapM_ print [ varsOf (TVar 0) == [0]
              ]

  putStr "\n"
  putStrLn "--- stackVarsOf ---"
  mapM_ print [ stackVarsOf (Fun s t) == [0,1]
              ]

  putStr "\n"
  putStrLn "--- isUnified ---"
  mapM_ print [ isUnified (Fun s ([TVar 0, TVar 0] :# 0))
              ]

  putStr "\n"
  putStrLn "--- cyclical types ---"
  mapM_ print [ testUnifier [Dup,I] == Nothing
              ]

  putStr "\n"
  putStrLn "--- full inference test ---"
  mapM_ print [ testUnifier []
              , testUnifier [Dup]
              , testUnifier [Dup, Dup]
              , testUnifier $ replicate 10 Dup
              , testUnifier [I, Dup]
              ]
