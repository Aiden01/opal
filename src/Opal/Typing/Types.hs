{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}
module Opal.Typing.Types
  ( TypeCheckEnv(..)
  , tExpected
  , locals
  , subst
  , supply
  , TypeError(..)
  , TypeCheck
  , Checkable(..)
  )
where

import           Control.Monad.Reader
import           Control.Lens
import           Opal.Parsing.AST               ( Type(..) )
import           Opal.Common
import           Control.Monad.Except           ( ExceptT )

data TypeCheckEnv = TypeCheckEnv
  { _tExpected   :: Maybe Type
  , _locals      :: Scope Type
  , _subst       :: Scope Type
  , _supply      :: Int
  }
makeLenses ''TypeCheckEnv

data TypeError
  = NotInScope Name
  | Custom String
  -- expected - given
  | Mismatch Type Type

instance Show TypeError where
  show (NotInScope x) = "Variable " <> x <> " is not defined"
  show (Custom error) = error
  show (Mismatch t1 t2) = "Expected type " <> show t1 <> " but found type " <> show t2

type TypeCheck = ReaderT TypeCheckEnv (ExceptT TypeError IO)

class Checkable a b | a -> b where
  getType :: a -> TypeCheck b
