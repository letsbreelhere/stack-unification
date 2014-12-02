module Unification where

import Control.Monad.State (runStateT)
import Types (Equation(..),Type(..),StackType(..))
import Unification.Types

unify :: Type -> [Equation] -> Maybe Type
unify t es = fmap fst $ untilSameOn (fmap fst) (unifyIter es t)

untilSameOn :: Eq b => (a -> b) -> [a] -> a
untilSameOn f (x:x':xs) = if (f x == f x') then x else untilSameOn f (x':xs)

unifyIter :: [Equation] -> Type -> [Maybe (Type, [Equation])]
unifyIter es t = case runStateT (runUnifier (unify' es t)) es of
  Just (t',es') -> Just (t',es') : unifyIter es' t'
  Nothing -> repeat Nothing

unify' :: [Equation] -> Type -> Unifier Type
unify' [] t = return t
unify' ((s :~ s'):es) t = subst s s' t >>= unify' es
