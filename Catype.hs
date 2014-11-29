{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Catype where

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List
import Data.Tuple
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

showStack = map toUpper . show . TVar

(-->) :: [Type] -> [Type] -> Inference Type
s --> t = do n <- freshStackVar
             return $ Fun (s :# n) (t :# n)

data StackType = [Type] :# Int
  deriving (Eq)

instance Show StackType where
  show (as :# a) = unwords $ (map show as) ++ [showStack a]

data Equation = TypeEquation Type Type
              | StackEquation Int StackType
              deriving (Eq)

instance Show Equation where
  show (TypeEquation t t') = show t ++ " = " ++ show t'
  show (StackEquation n s) = showStack n ++ " = " ++ show s

data IState =
  IState { _varMax :: Int
         , _stackVarMax :: Int
         , _eqns :: [Equation]
         }
  deriving (Show)

type Inference a = StateT IState Maybe a

addEqn l r = modify $ \s -> s { _eqns = TypeEquation l r : _eqns s}
addStackEqn l r = modify $ \s -> s { _eqns = StackEquation l r : _eqns s}

addStackConstraints ([] :# l) (rs :# r) = addStackEqn l (rs :# r)
addStackConstraints (ls :# l) ([] :# r) = addStackEqn r (ls :# l)
addStackConstraints ((lv:ls) :# l) ((rv:rs) :# r) = addEqn lv rv >> addStackConstraints (ls :#l) (rs :#r)

incVarPool :: Int -> Inference ()
incVarPool k = modify $ \s -> s { _varMax = _varMax s + k }

incStackVarPool :: Int -> Inference ()
incStackVarPool k = modify $ \s -> s { _stackVarMax = _stackVarMax s + k }

defaultState = IState 0 0 []

freshVar = TVar <$> (gets _varMax <* incVarPool 1)
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
                          addStackConstraints b c
                          return (Fun a d)

prependInitialType :: Type -> Inference ()
prependInitialType t = do v <- freshVar
                          addEqn v t

inferType e = flip runStateT defaultState $ do
  t <- typeOf e
  prependInitialType t
  return t

substituteType :: Int -> Type -> Type -> Type
substituteType k t t' = case t' of
  Concrete -> Concrete
  TVar n -> if n == k then t else t'
  Fun (ls :# l) (rs :# r) -> Fun (map (substituteType k t) ls :# l) (map (substituteType k t) rs :# r)

substituteEquation :: Int -> Type -> Equation -> Equation
substituteEquation k t e = case e of
  TypeEquation u u' -> TypeEquation (substituteType k t u) (substituteType k t u')
  StackEquation n s -> StackEquation n (substituteStackType k t s)

substituteStack :: Int -> StackType -> Equation -> Equation
substituteStack n s e = case e of
  TypeEquation u u'   -> TypeEquation (substituteTypeStack n s u) (substituteTypeStack n s u')
  StackEquation n' s' -> StackEquation n' (substituteStackType' n s s')

substituteStackType :: Int -> Type -> StackType -> StackType
substituteStackType n t (as :# a) = map (substituteType n t) as :# a

substituteStackType' :: Int -> StackType -> StackType -> StackType
substituteStackType' n (us :# u) (as :# a) = let (as' :# a') = if a == n
                                                               then (as ++ us) :# u
                                                               else as :# a
                                               in map (substituteTypeStack n (us :# u)) as' :# a'

substituteTypeStack :: Int -> StackType -> Type -> Type
substituteTypeStack n s t = case t of
  Fun l r -> Fun (substituteStackType' n s l) (substituteStackType' n s r)
  _ -> t

occursIn :: Type -> Type -> Bool
occursIn l r = (l == r) || case r of
  Fun (as :# _) (bs :# _) -> any (occursIn l) (as ++ bs)
  _ -> False

stackOccursIn :: Int -> StackType -> Bool
stackOccursIn n (s :# n') = n == n' || any stackOccursIn' s
  where stackOccursIn' t = case t of
          Fun l r -> n `stackOccursIn` l || n `stackOccursIn` r
          _ -> False

unifyFolder :: Equation -> [Equation] -> Maybe [Equation]
unifyFolder e es = case e of
  TypeEquation t t'  -> typeUnifyFolder t t' es
  StackEquation n s' -> return $ map (substituteStack n s') es `union` [e]

typeUnifyFolder :: Type -> Type -> [Equation] -> Maybe [Equation]
typeUnifyFolder t t' es = let e = TypeEquation t t' in case (t,t') of
  (v@(TVar n), _) | v == t' -> return es
                  | v `occursIn` t' -> fail $ "Occurs check failed; could not create cyclic type " ++ show e
                  | otherwise -> return $ map (substituteEquation n t') es `union` [e]
  (_, TVar _) -> typeUnifyFolder t' t es
  (Fun (ls :# l) (rs :# r), Fun (ls' :# l') (rs' :# r')) -> let es' = unifyStacks es (ls :# l) (ls' :# l')
                                                            in return $ unifyStacks es' (rs :# r) (rs' :# r')
  (Concrete, Concrete) -> return es
  (_, Concrete) -> fail $ "Couldn't match type" ++ show Concrete ++ " with " ++ show t
  (Concrete, _) -> fail $ "Couldn't match type" ++ show Concrete ++ " with " ++ show t'

unifyStacks :: [Equation] -> StackType -> StackType -> [Equation]
unifyStacks es ([] :# a) b = map (substituteStack a b) es `union` [StackEquation a b]
unifyStacks es (as :# a) ([] :# b) = unifyStacks es ([] :# b) (as :# a)
unifyStacks es ((a':as) :# a) ((b':bs) :# b) = let rest = unifyStacks es (as :# a) (bs :# b)
                                               in es `union` [TypeEquation a' b'] `union` rest

unifyStep :: [Equation] -> Maybe [Equation]
unifyStep es = case es of
  [] -> return []
  (e:es') -> unifyFolder e es'

unification :: CExp -> [Maybe [Equation]]
unification e = let Just (_,s) = inferType e
                    es = _eqns s
                in iterate (>>= unifyStep) (return es)
