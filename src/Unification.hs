module Unification where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.State (get,put,modify, runStateT)
import Types (Equation(..),Type(..),StackType(..))
import Unification.Types (Unifier(..))

addEqn :: Equation -> Unifier ()
addEqn e = Unifier (modify (++[e]))

getEqns :: Unifier [Equation]
getEqns = Unifier get

clearEqns :: Unifier ()
clearEqns = Unifier (put [])

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
  ([] :# a, _) -> substituteStackInType a s' t
  (_, [] :# _) -> unifyStep t (s' :~ s)
  ((l:as) :# a, (r:bs) :# b) -> do es <- getEqns
                                   clearEqns
                                   es' <- mapM (substituteTypeInEquation l r) es
                                   mapM_ addEqn es'
                                   t' <- substituteTypeInType l r t
                                   t'' <- unifyStep t' ((as :# a) :~ (bs :# b))
                                   return t''

substituteTypeInType :: Type -> Type -> Type -> Unifier Type
substituteTypeInType l r t
  | l == r = return t
  | l `occursIn` r = fail "Cyclic type"
  | otherwise = case (l,r) of
                  (TVar n,_) -> return $ substituteTVarInType n r t
                  (_,TVar _) -> substituteTypeInType r l t
                  (Fun a b, Fun c d) -> do mapM_ addEqn [a :~ c, b :~ d]
                                           return t
                  _ -> return t

substituteTVarInType :: Int -> Type -> Type -> Type
substituteTVarInType n r t = case t of
  TVar n' | n == n'   -> r
          | otherwise -> t
  Fun (as :# a) (bs :# b) -> let as' = map (substituteTVarInType n r) as
                                 bs' = map (substituteTVarInType n r) bs
                             in Fun (as' :# a) (bs' :# b)
  _ -> t

occursIn :: Type -> Type -> Bool
occursIn t t'
  | t == t' = True
  | otherwise = case t' of
                  Fun (as :# _) (bs :# _) -> any (t `occursIn`) (as ++ bs)
                  _ -> False

stackOccursIn :: Int -> StackType -> Bool
stackOccursIn n (as :# a) = n == a || any stackOccursIn' as
  where stackOccursIn' t = case t of
          Fun (xs :# x) (ys :# y) -> n == x || n == y || any stackOccursIn' (xs++ys)
          _ -> False

substituteTypeInEquation :: Type -> Type -> Equation -> Unifier Equation
substituteTypeInEquation l r (a :~ b) = do a' <- substituteTypeInStack l r a
                                           b' <- substituteTypeInStack l r b
                                           return $ a' :~ b'

substituteTypeInStack :: Type -> Type -> StackType -> Unifier StackType
substituteTypeInStack l r (as :# a) = do as' <- mapM (substituteTypeInType l r) as
                                         return (as' :# a)

substituteStackInType :: Int -> StackType -> Type -> Unifier Type
substituteStackInType n a _ | n `stackOccursIn` a = fail "Cyclic type"
substituteStackInType n a t = case t of
  Fun b c -> Fun <$> (substituteStackInStack n a b) <*> (substituteStackInStack n a c)
  _ -> return t

substituteStackInStack :: Int -> StackType -> StackType -> Unifier StackType
substituteStackInStack n (as :# a) (xs :# x) = do
  xs' <- mapM (substituteStackInType n (as :# a)) xs
  return $ if x == n
    then (xs' ++ as) :# a
    else xs' :# x

substituteStackInEquation :: Int -> StackType -> Equation -> Unifier Equation
substituteStackInEquation n s (a :~ b) = (:~) <$> substituteStackInStack n s a <*> substituteStackInStack n s b
