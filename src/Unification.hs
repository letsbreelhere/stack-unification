module Unification where

import Types (Equation(..),Type(..))
import Unification.Types

unify :: Type -> [Equation] -> Maybe Type
unify t es = fmap fst . untilSameOn fst $ unifyIter es t

untilSameOn :: Eq b => (a -> b) -> [a] -> Maybe a
untilSameOn f (x:x':xs) = if f x == f x' then Just x else untilSameOn f (x':xs)
untilSameOn _ _ = Nothing

unifyIter :: [Equation] -> Type -> [(Type, [Equation])]
unifyIter es t = case runUnifier (unifyStep es t) es of
  Just (t',es') -> (t',es') : unifyIter es' t'
  Nothing -> []

unifyStep :: [Equation] -> Type -> Unifier Type
unifyStep es t = case es of
  [] -> return t
  (s :~ s') : es' -> subst s s' t >>= unifyStep es'
