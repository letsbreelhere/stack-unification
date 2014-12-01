{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unification.Types ( Unifier(..)
                         , Substitutable(..)
                         , addEqn
                         , getEqns
                         , clearEqns
                         ) where

import Types

import Control.Applicative
import Control.Monad.State (StateT, modify, get, put)

newtype Unifier a = Unifier { runUnifier :: StateT [Equation] Maybe a }
  deriving (Functor, Applicative, Monad)

class Substitutable t where
  substType :: Type -> Type -> t -> Unifier t
  substStack :: Int -> StackType -> t -> Unifier t

instance Substitutable Type where
  substType l r t
    | l == r = return t
    | l `occursIn` r = fail "Cyclic type"
    | otherwise = case (l,r) of
                    (TVar n,_) -> return $ substituteTVarInType n r t
                    (_,TVar _) -> substType r l t
                    (Fun a b, Fun c d) -> do addEqn (a :~ c)
                                             addEqn (b :~ d)
                                             return t
                    _ -> return t
  substStack n a t | n `stackOccursIn` a = fail "Cyclic type"
                   | otherwise = case t of
                       Fun b c -> Fun <$> substStack n a b <*> substStack n a c
                       _ -> return t

instance Substitutable StackType where
  substType l r (as :# a) = do as' <- mapM (substType l r) as
                               return (as' :# a)
  substStack n (as :# a) (xs :# x) = do
    xs' <- mapM (substStack n (as :# a)) xs
    return $ if x == n
      then (xs' ++ as) :# a
      else xs' :# x

instance Substitutable Equation where
  substStack n s (a :~ b) = (:~) <$> substStack n s a <*> substStack n s b
  substType l r  (a :~ b) = (:~) <$> substType l r a  <*> substType l r b

addEqn :: Equation -> Unifier ()

addEqn e = Unifier (modify (++[e]))

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

substituteTVarInType :: Int -> Type -> Type -> Type
substituteTVarInType n r t = case t of
  TVar n' | n == n'   -> r
          | otherwise -> t
  Fun (as :# a) (bs :# b) -> let as' = map (substituteTVarInType n r) as
                                 bs' = map (substituteTVarInType n r) bs
                             in Fun (as' :# a) (bs' :# b)
  _ -> t

getEqns :: Unifier [Equation]
getEqns = Unifier get

clearEqns :: Unifier ()
clearEqns = Unifier (put [])
