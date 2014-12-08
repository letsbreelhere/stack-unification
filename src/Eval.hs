module Eval where

import Types
import Eval.Types

eval :: CExp -> Stack CExp -> IO (Either EvalError (Stack CExp))
eval = undefined
