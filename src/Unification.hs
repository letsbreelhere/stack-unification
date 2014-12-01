module Unification where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.State (get,put,modify, runStateT)
import Types (Equation(..),Type(..),StackType(..))
import Unification.Types

unify :: Type -> [Equation] -> Maybe Type
unify t es = untilSame (unifyIter es t)

untilSame :: Eq a => [a] -> a
untilSame (x:x':xs) = if x == x' then x else untilSame (x':xs)

unifyIter :: [Equation] -> Type -> [Maybe Type]
unifyIter es t = h : rest
  where (h,rest) = case runStateT (runUnifier (unify' es t)) es of
                     Just (t',es') -> (Just t',unifyIter es' t')
                     Nothing -> (Nothing, repeat Nothing)

unify' :: [Equation] -> Type -> Unifier Type
unify' [] t = return t
unify' (e:es) t = unifyStep t e >>= unify' es

unifyStep :: Type -> Equation -> Unifier Type
unifyStep t e = let s :~ s' = e in case (s,s') of
  ([] :# a, _) -> subst a s' t
  (_, [] :# _) -> unifyStep t (s' :~ s)
  ((l:as) :# a, (r:bs) :# b) -> do es <- getEqns
                                   clearEqns
                                   es' <- mapM (subst l r) es
                                   mapM_ addEqn es'
                                   t' <- subst l r t
                                   t'' <- unifyStep t' ((as :# a) :~ (bs :# b))
                                   return t''
