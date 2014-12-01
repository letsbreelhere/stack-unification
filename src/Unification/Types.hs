{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Unification.Types where

import Types

import Control.Applicative
import Control.Monad.State

newtype Unifier a = Unifier { runUnifier :: StateT [Equation] Maybe a }
  deriving (Functor, Applicative, Monad)

class Subst l r t where
  subst :: l -> r -> t -> Unifier t

instance Subst Type Type Type where
  subst l r t
    | l == r = return t
    | l `occursIn` r = fail "Cyclic type"
    | otherwise = case (l,r) of
                    (TVar n,_) -> subst n r t
                    (_,TVar _) -> subst r l t
                    (Fun a b, Fun c d) -> do mapM_ addEqn [a :~ c, b :~ d]
                                             return t
                    _ -> return t

instance Subst Int Type Type where
  subst n r t = case t of
    TVar n' | n == n'   -> return r
            | otherwise -> return t
    Fun (as :# a) (bs :# b) -> do as' <- mapM (subst n r) as
                                  bs' <- mapM (subst n r) bs
                                  return $ Fun (as' :# a) (bs' :# b)
    _ -> return t

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

instance Subst Type Type Equation where
  subst l r (a :~ b) = do a' <- subst l r a
                          b' <- subst l r b
                          return $ a' :~ b'

instance Subst Type Type StackType where
  subst l r (as :# a) = do as' <- mapM (subst l r) as
                           return (as' :# a)

instance Subst Int StackType Type where
  subst n a t | n `stackOccursIn` a = fail "Cyclic type"
              | otherwise = case t of
                  Fun b c -> Fun <$> (subst n a b) <*> (subst n a c)
                  _ -> return t

instance Subst Int StackType StackType where
  subst n (as :# a) (xs :# x) = do
    xs' <- mapM (subst n (as :# a)) xs
    return $ if x == n
      then (xs' ++ as) :# a
      else xs' :# x

instance Subst Int StackType Equation where
  subst n s (a :~ b) = (:~) <$> subst n s a <*> subst n s b

addEqn :: Equation -> Unifier ()
addEqn e = Unifier (modify (++[e]))

getEqns :: Unifier [Equation]
getEqns = Unifier get

clearEqns :: Unifier ()
clearEqns = Unifier (put [])