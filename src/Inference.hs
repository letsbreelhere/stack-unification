module Inference ( inferType
                 ) where

import Control.Arrow
import Control.Applicative
import Control.Lens
import Control.Monad.State (runStateT)
import Inference.Types
import Types

(-->) :: [Type] -> [Type] -> Inference Type
s --> t = do n <- freshStackVar
             return $ Fun (s :# n) (t :# n)


addConstraint :: StackType -> StackType -> Inference ()
addConstraint l r = eqns %= ((l :~ r) :)

freshVar :: Inference Type
freshVar = TVar <$> (varMax %%= (id &&& succ))

freshStackVar :: Inference Int
freshStackVar = stackVarMax %%= (id &&& succ)

typeOf :: CExp -> Inference Type
typeOf Pop      = do v <- freshVar
                     [v] --> []
typeOf Dup      = do v <- freshVar
                     [v] --> [v, v]
typeOf Swap     = do v <- freshVar
                     v' <- freshVar
                     [v, v'] --> [v', v]
typeOf SomePush = [] --> [Concrete]
typeOf I        = do s <- freshStackVar
                     s' <- freshStackVar
                     let top = Fun ([] :# s) ([] :# s')
                     return (Fun ([top] :# s) ([] :# s'))
typeOf (Compose l r) = do Fun a b <- typeOf l
                          Fun c d <- typeOf r
                          addConstraint b c
                          return (Fun a d)
typeOf (Quote e) = do t <- typeOf e
                      [] --> [t]
typeOf SomeValue = return Concrete
typeOf Empty = [] --> []

inferType :: CExp -> Maybe (Type,[Equation])
inferType e = do (t,s) <- runStateT (typeOf e) defaultState
                 return (t,s^.eqns)
