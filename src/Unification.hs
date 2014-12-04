module Unification (unify) where

import Control.Applicative
import Types (Equation(..),Type(..))
import Unification.Types

unify :: Type -> [Equation] -> Either String Type
unify t es = fst <$> runUnifier (unify' t) es

unify' :: Type -> Unifier Type
unify' t = do
  es <- getEqns
  case es of
    [] -> return t
    e@(s :~ s') : _ -> do
      t' <- subst s s' t
      deleteEqn e
      unify' t'
