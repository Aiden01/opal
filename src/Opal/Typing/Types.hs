{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}
module Opal.Typing.Types
  ( TypeCheckEnv
  , tExpected
  , locals
  , TypeError(..)
  , TypeCheck
  , Checkable(..)
  )
where

import           Control.Monad.Reader
import           Control.Lens
import           Opal.Parsing.AST               ( Type )
import           Opal.Common
import           Control.Monad.Except           ( ExceptT )

data TypeCheckEnv = TypeCheckEnv
  { _tExpected   :: Maybe Type
  , _locals      :: Scope Type
  }
makeLenses ''TypeCheckEnv

data TypeError
  = NotInScope Name
  | Custom String
  -- expected - given
  | Mismatch Type Type deriving (Show)

type TypeCheck = ReaderT TypeCheckEnv (ExceptT TypeError IO)

class Checkable a b | a -> b where
  getType :: a -> TypeCheck b
