module Unification where

import Data.List (union)
import Debug.Trace
import Types (Equation(..),Type(..))
import Unification.Types

unify :: Type -> [Equation] -> Maybe Type
unify t es = fmap fst $ runUnifier (unify' t) es

unify' :: Type -> Unifier Type
unify' t = do
  es <- getEqns
  case es of
    [] -> return t
    e@(s :~ s') : _ -> do
      t' <- subst s s' t
      deleteEqn e
      unify' t'
