{-# LANGUAGE TemplateHaskell #-}

module Inference.Types ( CExp(..)
                       , IState
                       , defaultState
                       , Inference
                       , varMax
                       , stackVarMax
                       , eqns
                       , env
                       , (-->)
                       , freshVar
                       , freshStackVar
                       , addConstraint
                       , lookupType
                       , failWith
                       ) where

import Types
import Control.Applicative
import Control.Arrow
import Control.Monad.State (StateT, lift)
import Control.Lens
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type Inference a = StateT IState (Either String) a

data IState =
  IState { _varMax :: Int
         , _stackVarMax :: Int
         , _eqns :: [Equation]
         , _env :: Map String (Inference Type)
         }

makeLenses ''IState

defaultState :: IState
defaultState = IState 0 0 [] Map.empty

(-->) :: [Type] -> [Type] -> Inference Type
s --> t = do n <- freshStackVar
             return $ Fun (s :# n) (t :# n)

failWith :: String -> Inference a
failWith = lift . Left

lookupType :: String -> Inference Type
lookupType s = do
  e <- use env
  let mt = Map.lookup s e
      failure = failWith $ "Undefined term `" ++ s ++ "'"
  fromMaybe failure mt

addConstraint :: StackType -> StackType -> Inference ()
addConstraint l r = eqns %= ((l :~ r) :)

freshVar :: Inference Type
freshVar = TVar <$> (varMax %%= (id &&& succ))

freshStackVar :: Inference Int
freshStackVar = stackVarMax %%= (id &&& succ)
