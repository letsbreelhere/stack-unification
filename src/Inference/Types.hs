{-# LANGUAGE TemplateHaskell #-}

module Inference.Types ( CExp(..)
                       , IState
                       , defaultState
                       , Inference
                       , varMax
                       , stackVarMax
                       , eqns
                       ) where

import Types
import Control.Monad.State (StateT)
import Control.Lens (makeLenses)

data CExp = Pop
          | Dup
          | Swap
          | SomePush
          | SomeValue
          | I
          | Compose CExp CExp
          | Quote CExp
          | Empty

instance Show CExp where
  show Pop = "pop"
  show Dup = "dup"
  show Swap = "swap"
  show SomePush = "push"
  show SomeValue = "1"
  show I = "i"
  show (Compose a b) = unwords [show a, show b]
  show (Quote e) = "[" ++ show e ++ "]"
  show Empty = ""

data IState =
  IState { _varMax :: Int
         , _stackVarMax :: Int
         , _eqns :: [Equation]
         }
  deriving (Show)

makeLenses ''IState

defaultState :: IState
defaultState = IState 0 0 []

type Inference a = StateT IState (Either String) a
