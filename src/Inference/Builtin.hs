module Inference.Builtin (builtinTypes) where

import Types
import Inference.Types

builtinTypes :: [(String, Inference Type)]
builtinTypes = [ ("dup", dup_)
               , ("i", i_)
               , ("drop", drop_)
               , ("swap", swap_)
               , ("quote", quote_)
               ]

dup_ :: Inference Type
dup_ = do v <- freshVar
          [v] --> [v, v]

drop_ :: Inference Type
drop_ = do v <- freshVar
           [v] --> []

swap_ :: Inference Type
swap_ = do v <- freshVar
           v' <- freshVar
           [v, v'] --> [v', v]

i_ :: Inference Type
i_ = do s <- freshStackVar
        s' <- freshStackVar
        let top = Fun ([] :# s) ([] :# s')
        return $ Fun ([top] :# s) ([] :# s')

quote_ :: Inference Type
quote_ = do v <- freshVar
            f <- [] --> [v]
            [v] --> [f]
