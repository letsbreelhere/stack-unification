{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Catype (CExp(..), unifyType) where

import Control.Applicative
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
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

data Equation = TypeEquation Type Type
              | StackEquation StackType StackType
              deriving (Eq)

instance Show Equation where
  show (TypeEquation t t') = show t ++ " = " ++ show t'
  show (StackEquation n s) = show n ++ " = " ++ show s

data IState =
  IState { _varMax :: Int
         , _stackVarMax :: Int
         , _eqns :: [Equation]
         }
  deriving (Show)

type Inference a = StateT IState Maybe a

addEqn :: Type -> Type -> Inference ()
addEqn l r = modify $ \s -> s { _eqns = TypeEquation l r : _eqns s}

addStackEqn :: StackType -> StackType -> Inference ()
addStackEqn l r = modify $ \s -> s { _eqns = StackEquation l r : _eqns s}

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
                          addStackEqn b c
                          return (Fun a d)
typeOf (Quote e) = do t <- typeOf e
                      [] --> [t]
typeOf SomeValue = return Concrete
typeOf Empty = [] --> []

inferType :: CExp -> Maybe (Int,IState)
inferType e = flip runStateT defaultState $ do
  t <- typeOf e
  v@(TVar n) <- freshVar
  addEqn v t
  return n

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

type Unification = Either String [Equation]

unifyFolder :: Equation -> [Equation] -> Unification
unifyFolder e es = case e of
  TypeEquation t t'  -> typeUnifyFolder t t' es
  StackEquation s s' -> return $ unifyStacks es s s'

typeUnifyFolder :: Type -> Type -> [Equation] -> Unification
typeUnifyFolder t t' es = let e = TypeEquation t t' in case (t,t') of
  (v@(TVar n), _) | v == t' -> return es
                  | v `occursIn` t' -> Left $ "Occurs check failed; could not create cyclic type " ++ show e
                  | otherwise -> return $ map (substituteEquation n t') es `union` [e]
  (_, TVar _) -> typeUnifyFolder t' t es
  (Fun (ls :# l) (rs :# r), Fun (ls' :# l') (rs' :# r')) -> let es' = unifyStacks es (ls :# l) (ls' :# l')
                                                            in return $ unifyStacks es' (rs :# r) (rs' :# r')
  (Concrete, Concrete) -> return es
  (_, Concrete) -> Left $ "Couldn't match type" ++ show Concrete ++ " with " ++ show t
  (Concrete, _) -> Left $ "Couldn't match type" ++ show Concrete ++ " with " ++ show t'

unifyStacks :: [Equation] -> StackType -> StackType -> [Equation]
unifyStacks es e@([] :# a) b = map (substituteStack a b) es `union` [StackEquation e b]
unifyStacks es (as :# a) ([] :# b) = unifyStacks es ([] :# b) (as :# a)
unifyStacks es ((a':as) :# a) ((b':bs) :# b) = let rest = unifyStacks es (as :# a) (bs :# b)
                                               in es `union` [TypeEquation a' b'] `union` rest

unifyStep :: [Equation] -> Unification
unifyStep es = case es of
  [] -> return []
  (e:es') -> unifyFolder e es'

unifications :: CExp -> [Unification]
unifications e = fromMaybe [] $ do
  (_,s) <- inferType e
  let es = _eqns s
  return $ iterate (>>= unifyStep) (return es)

unifyType :: CExp -> Either String Type
unifyType = fmap solution . findEither isUnifier "This should not happen" . unifications
  where solution = rhs . maximumBy (compare `on` lhsSize)
        rhs (TypeEquation  _ r) = r
        rhs (StackEquation _ _) = error "Nope"
        lhsSize (TypeEquation (TVar n) _) = n
        lhsSize _ = -1

findEither :: (b -> Bool) -> a -> [Either a b] -> Either a b
findEither p err xs = case xs of
  []        -> Left err
  Left y:_  -> Left y
  Right y:ys | p y -> Right y
             | otherwise -> findEither p err ys

isUnifier :: [Equation] -> Bool
isUnifier es = undefined
