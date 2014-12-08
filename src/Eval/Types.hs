{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Eval.Types ( Stack
                  , Eval
                  , EvalError
                  , runEval
                  , stack
                  , definitions
                  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.Error

import Data.Map (Map)

import Types

newtype Stack a = Stack { unStack :: [a] }
  deriving (Eq)

instance Show a => Show (Stack a) where
  show = show . unStack

newtype EvalError = EvalError String
  deriving (Error)

instance Show EvalError where
  show (EvalError err) = err

data EvalState =
  EvalState { _stack :: Stack CExp
            , _definitions :: Map String CExp
            }
makeLenses ''EvalState

newtype Eval a = Eval { runEval :: StateT EvalState (ErrorT EvalError IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           )
