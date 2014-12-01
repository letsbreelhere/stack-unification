{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Catype where

import Control.Applicative
import Data.Char
import Data.List
import Debug.Trace
import Control.Monad.State

data CExp = Pop
          | Dup
          | Swap
          | SomePush
          | SomeValue
          | I
          | Compose CExp CExp
          | Quote CExp
          | Empty
          deriving (Show)

data Type = TVar Int
          | Concrete
          | Fun StackType StackType
  deriving (Eq)

instance Show Type where
  show (TVar n)
    | n < 26    = [toEnum (fromEnum n + fromEnum 'a')]
    | otherwise = reverse $ show (TVar (n `mod` 26)) ++ show (TVar $ n `div` 26 - 1)
  show Concrete = "int"
  show (Fun (as :# a) (bs :# b)) = "(" ++ unwords (map show as ++ [showStack a]) ++ " -> " ++ unwords (map show bs ++ [showStack b]) ++ ")"

showStack :: Int -> String
showStack = map toUpper . show . TVar

(-->) :: [Type] -> [Type] -> Inference Type
s --> t = do n <- freshStackVar
             return $ Fun (s :# n) (t :# n)

data StackType = [Type] :# Int
  deriving (Eq)

instance Show StackType where
  show (as :# a) = unwords $ (map show as) ++ [showStack a]

data Equation = StackEquation StackType StackType
              deriving (Eq)

instance Show Equation where
  show (StackEquation n s) = show n ++ " = " ++ show s

data IState =
  IState { _varMax :: Int
         , _stackVarMax :: Int
         , _eqns :: [Equation]
         }
  deriving (Show)

type Inference a = StateT IState Maybe a

addConstraint :: StackType -> StackType -> Inference ()
addConstraint l r = modify $ \s -> s { _eqns = StackEquation l r : _eqns s}

incVarPool :: Int -> Inference ()
incVarPool k = modify $ \s -> s { _varMax = _varMax s + k }

incStackVarPool :: Int -> Inference ()
incStackVarPool k = modify $ \s -> s { _stackVarMax = _stackVarMax s + k }

defaultState :: IState
defaultState = IState 0 0 []

freshVar :: Inference Type
freshVar = TVar <$> (gets _varMax <* incVarPool 1)

freshStackVar :: Inference Int
freshStackVar = gets _stackVarMax <* incStackVarPool 1

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
                 return (t,_eqns s)

type Unifier a = StateT [Equation] Maybe a

unify :: Type -> [Equation] -> Maybe Type
unify t es = do (t', es') <- runStateT (unify' es t) es
                trace (show es') (return ())
                if isUnified t'
                  then return t'
                  else unify t' es'

unify' :: [Equation] -> Type -> Unifier Type
unify' [] t = return t
unify' (e:es) t = unifyStep t e >>= unify' es

isUnified :: Type -> Bool
isUnified t = case t of
  Fun (as :# a) (bs :# b) -> null (nub (concatMap varsOf bs) \\ nub (concatMap varsOf as)) &&
                             null (nub (a : concatMap stackVarsOf bs) \\ nub (b : concatMap stackVarsOf as))
  _ -> True

varsOf t = case t of
  TVar k -> [k]
  Fun (as :# a) (bs :# b) -> varsOf =<< (as++bs)
  _ -> []

stackVarsOf t = case t of
  Fun (as :# a) (bs :# b) -> a : b : (stackVarsOf =<< (as++bs))
  _ -> []

unifyStep :: Type -> Equation -> Unifier Type
unifyStep t e = let StackEquation s s' = e in case (s,s') of
  ([] :# a, _) -> substituteStackInType a s' t
  (_, [] :# b) -> unifyStep t (StackEquation s' s)
  ((l:as) :# a, (r:bs) :# b) -> do es <- get
                                   put []
                                   es' <- mapM (substituteTypeInEquation l r) es
                                   modify (union es')
                                   t' <- substituteTypeInType l r t
                                   t'' <- unifyStep t' (StackEquation (as :# a) (bs :# b))
                                   return t''

substituteTypeInType :: Type -> Type -> Type -> Unifier Type
substituteTypeInType l r t
  | l == r = return t
  | l `occursIn` r = fail "Cyclic type"
  | otherwise = case (l,r) of
                  (TVar n,_) -> return $ substituteTVarInType n r t
                  (_,TVar _) -> substituteTypeInType r l t
                  (Fun a b, Fun c d) -> do modify (`union` [StackEquation a c, StackEquation b d])
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
substituteTypeInEquation l r (StackEquation a b) = do a' <- substituteTypeInStack l r a
                                                      b' <- substituteTypeInStack l r b
                                                      return $ StackEquation a' b'

substituteTypeInStack :: Type -> Type -> StackType -> Unifier StackType
substituteTypeInStack l r (as :# a) = do as' <- mapM (substituteTypeInType l r) as
                                         return (as' :# a)

substituteStackInType :: Int -> StackType -> Type -> Unifier Type
substituteStackInType n a t | n `stackOccursIn` a = fail "Cyclic type"
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
substituteStackInEquation n s (StackEquation a b) = StackEquation <$> substituteStackInStack n s a <*> substituteStackInStack n s b
