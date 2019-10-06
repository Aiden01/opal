{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Opal.Codegen
  ()
where

import           LLVM.AST                       ( Type(..)
                                                , Operand
                                                , FloatingPointType(..)
                                                , Name
                                                , Named
                                                , Instruction
                                                , Terminator
                                                , Module(..)
                                                , defaultModule
                                                , Definition
                                                )
import qualified Data.Map                      as M
import           Opal.Common                    ( Scope )
import           Control.Monad.State
import           Data.ByteString.Short

type SymbolTable = [(String, Operand)]
type Names = Scope Int

data CodegenState = CodegenState
  { currentBlock :: Name
  , blocks :: M.Map Name BlockState
  , symbolTable :: SymbolTable
  , blockCount :: Int
  , count :: Word
  , names :: Names
  } deriving Show

data BlockState = BlockState
  { idx :: Int
  , stack :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

newtype LLVM a = LLVM (State Module a)
  deriving (Functor, Applicative, Monad, MonadState Module )

runLLVM :: Module -> LLVM a -> Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

float :: Type
float = FloatingPointType DoubleFP

int :: Type
int = IntegerType 64








