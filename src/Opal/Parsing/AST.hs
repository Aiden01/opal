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

type Name = String
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
  | TChar
  | TVar Name
  | TList [Type]
  | TFn Type [Type] -- return type and params type
  deriving (Show)

newtype Param = Param (Name, Type) deriving (Show)

data FnDecl = FnDecl Name [Param] Expr deriving (Show)
newtype Program = Program [FnDecl] deriving (Show)
