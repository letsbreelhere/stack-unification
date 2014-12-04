module Types where

import Data.Char (toUpper)
import Data.Monoid

data StackType = [Type] :# Int
  deriving (Eq)

data CExp = Term String
          | Bool Bool
          | Int Int
          | Char Char
          | String String
          | Compose CExp CExp
          | Quote CExp
          | Empty

instance Show CExp where
  show (Term s) = s
  show (Compose a b) = case (a,b) of
    (Empty,_) -> show b
    (_,Empty) -> show a
    _ -> show a ++ " " ++ show b
  show (Quote e) = "[" ++ show e ++ "]"
  show Empty = ""
  show (Bool b) = if b then "#T" else "#F"
  show (Int i) = show i
  show (Char c) = ['`', c, '`']
  show (String s) = show s

instance Monoid CExp where
  mappend = Compose
  mempty = Empty

data Type = TVar Int
          | Scalar String
          | Fun StackType StackType
  deriving (Eq)

instance Show Type where
  show (TVar n)
    | n < 26    = [toEnum (fromEnum n + fromEnum 'a')]
    | otherwise = reverse $ show (TVar (n `mod` 26)) ++ show (TVar $ n `div` 26 - 1)
  show (Scalar t) = t
  show (Fun a b) = show a ++ " -> " ++ show b

showStack :: Int -> String
showStack = map toUpper . show . TVar

instance Show StackType where
  show (as :# a) = unwords $ showStack a : map showInner (reverse as)
    where showInner t = case t of
            Fun _ _ -> "(" ++ show t ++ ")"
            _ -> show t

data Equation = StackType :~ StackType
  deriving (Eq)

instance Show Equation where
  show (n :~ s) = show n ++ " = " ++ show s
