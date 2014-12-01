module Types where

import Data.Char (toUpper)

data StackType = [Type] :# Int
  deriving (Eq)

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

instance Show StackType where
  show (as :# a) = unwords $ showStack a : map show as

data Equation = StackType :~ StackType
              deriving (Eq)

instance Show Equation where
  show (n :~ s) = show n ++ " = " ++ show s
