{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module Unification.Types where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Data.List

import Types

newtype Unifier a = Unifier { unUnifier :: StateT [Equation] Maybe a }
  deriving (Functor, Applicative, Monad)

runUnifier :: Unifier a -> [Equation] -> Maybe (a, [Equation])
runUnifier = runStateT . unUnifier

class Subst l r t where
  subst :: l -> r -> t -> Unifier t

instance Subst Type Type Type where
  subst l r t
    | l == r = return t
    | otherwise = case (l,r) of
                    (TVar n,_) | n `occursIn` r -> fail $ "Cyclic type " ++ show l ++ " = " ++ show r
                               | otherwise -> subst n r t
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

occursIn :: Int -> Type -> Bool
occursIn n t = case t of
  TVar m -> n == m
  Fun (as :# _) (bs :# _) -> any (n `occursIn`) (as ++ bs)
  _ -> False

stackOccursIn :: Int -> StackType -> Bool
stackOccursIn n (as :# a) = n == a || any stackOccursIn' as
  where stackOccursIn' t = case t of
          Fun (xs :# x) (ys :# y) -> n == x || n == y || any stackOccursIn' (xs++ys)
          _ -> False

instance Subst Type Type StackType where
  subst l r (as :# a) = do as' <- mapM (subst l r) as
                           return (as' :# a)

instance Subst Int StackType Type where
  subst n a@(xs :# x) t
    | n == x && null xs = return t
    | n `stackOccursIn` a = fail $ "Cyclic type " ++ showStack n ++ " = " ++ show a
    | otherwise = case t of
        Fun b c -> Fun <$> subst n a b <*> subst n a c
        _ -> return t

instance Subst Int StackType StackType where
  subst n (as :# a) (xs :# x) = do
    xs' <- mapM (subst n (as :# a)) xs
    return $ if x == n
      then (xs' ++ as) :# a
      else xs' :# x

instance Subst StackType StackType Type where
  subst s s' t | s == s' = return t
  subst s s' t = case (s,s') of
    ([] :# a, _) -> do substEqns a s'
                       subst a s' t
    (_, [] :# _) -> subst s' s t
    ((l:as) :# a, (r:bs) :# b) -> do substEqns l r
                                     subst l r t >>= subst (as :# a) (bs :# b)

instance Subst Int StackType Equation where
  subst n s (a :~ b) = (:~) <$> subst n s a <*> subst n s b

instance Subst Type Type Equation where
  subst l r (a :~ b) = (:~) <$> subst l r a <*> subst l r b

addEqn :: Equation -> Unifier ()
addEqn e = Unifier (modify (`union` [e]))

getEqns :: Unifier [Equation]
getEqns = Unifier get

clearEqns :: Unifier ()
clearEqns = Unifier (put [])

deleteEqn :: Equation -> Unifier ()
deleteEqn = Unifier . modify . delete

substEqns :: Subst l r Equation => l -> r -> Unifier ()
substEqns l r = do
  es <- getEqns
  clearEqns
  es' <- mapM (subst l r) es
  mapM_ addEqn es'
