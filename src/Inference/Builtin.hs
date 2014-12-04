module Inference.Builtin (builtinTypes) where

import Types
import Inference.Types

builtinTypes :: [(String, Inference Type)]
builtinTypes = [ ("dup", dup_)
               , ("i", i_)
               , ("pop", pop_)
               , ("swap", swap_)
               ]

dup_ :: Inference Type
dup_ = do v <- freshVar
          [v] --> [v, v]

pop_ :: Inference Type
pop_ = do v <- freshVar
          [v] --> []

swap_ :: Inference Type
swap_ = do v <- freshVar
           v' <- freshVar
           [v, v'] --> [v', v]

i_ :: Inference Type
i_ = do s <- freshStackVar
        s' <- freshStackVar
        let top = Fun ([] :# s) ([] :# s')
        return (Fun ([top] :# s) ([] :# s'))
