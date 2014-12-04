module Inference (inferType) where

import Control.Arrow
import Control.Applicative
import Control.Lens
import Control.Monad.State (runStateT)
import Inference.Types
import Inference.Builtin
import Types
import qualified Data.Map as Map

typeOf :: CExp -> Inference Type

typeOf (Term s) = lookupType s
typeOf (Compose l r) = do (a, b) <- typeOfFunction l
                          (c, d) <- typeOfFunction r
                          addConstraint b c
                          return (Fun a d)
typeOf (Bool _)   = [] --> [Scalar "bool"]
typeOf (Int _)    = [] --> [Scalar "int"]
typeOf (Char _)   = [] --> [Scalar "char"]
typeOf (String _) = [] --> [Scalar "string"]
typeOf (Quote e) = do t <- typeOf e
                      [] --> [t]
typeOf Empty = [] --> []

typeOfFunction :: CExp -> Inference (StackType, StackType)
typeOfFunction e = do
  t <- typeOf e
  case t of
    Fun a b -> return (a,b)
    _ -> failWith $ "Expected " ++ show e ++ " to be a function, but is actually " ++ show t

inferType :: CExp -> Either String (Type,[Equation])
inferType e = do (t,s) <- runStateT (typeOf e) defaultWithBuiltins
                 return (t,s^.eqns)

defaultWithBuiltins = defaultState & env .~ builtinEnv
  where builtinEnv = Map.fromList builtinTypes
