{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unification.Types where

import Types

import Control.Applicative
import Control.Monad.State (StateT)

newtype Unifier a = Unifier { runUnifier :: StateT [Equation] Maybe a }
  deriving (Functor, Applicative, Monad)
