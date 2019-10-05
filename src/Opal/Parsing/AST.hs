module Opal.Parsing.AST
  ( Expr(..)
  , Lit(..)
  , Stmt(..)
  , FnDecl(..)
  , Program(..)
  , Block
  , Param(..)
  , Type(..)
  , BinOP(..)
  , UnOP(..)
  , Name
  )
where

import           Opal.Common
import           Data.List                      ( intercalate )

type Block = [Stmt]

data Lit
  = LInt Integer
  | LString String
  | LChar Char
  | LBool Bool
  | LFloat Double deriving (Show)

data BinOP
  = BAnd
  | BOR
  | BEQ
  | BGT
  | BGTE
  | BLT
  | BLTE
  | BNotEQ
  | BDiv
  | BMult
  | BAdd
  | BSub
  | BPow deriving (Show)

data UnOP
  = UNot
  | UInc
  | UDecr deriving (Show)

data Expr
  = ECall Name [Expr]
  | ELit Lit
  | EList [Expr]
  | EBlock Block
  | ELam [Param] Expr
  | EBinOP BinOP Expr Expr -- Binary operators
  | EUnOP UnOP Expr -- Unary operators
  | EIf Expr Expr Expr -- if condition then expr else expr
  | EVar Name deriving (Show)

data Stmt
  = ExprStmt Expr
  | VarDeclStmt Name (Maybe Type) Expr
  | FnDeclStmt FnDecl deriving (Show)

data Type
  = TInt
  | TFloat
  | TString
  | TBool
  | TChar
  | TVar Name
  | TList Type
  | TFn Type [Type] -- return type and params type
  deriving Eq

instance Show Type where
  show TInt      = "Int"
  show TFloat    = "Float"
  show TString   = "String"
  show TChar     = "Char"
  show (TVar  x) = x
  show (TList t) = show t
  show (TFn ret params) =
    intercalate " -> " (map show params) <> " -> " <> show ret

newtype Param = Param (Name, Type) deriving (Show)

data FnDecl = FnDecl Name [Param] Expr deriving (Show)
newtype Program = Program [FnDecl] deriving (Show)
