{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances #-}
module Opal.Typing.TypeChecker
  ( typeCheck
  )
where

import           Opal.Common
import           Control.Lens
import           Opal.Typing.Types
import           Opal.Parsing.AST
import           Control.Monad.Except           ( throwError
                                                , runExceptT
                                                )
import           Data.Functor                   ( ($>) )
import           Control.Monad                  ( (>=>)
                                                , foldM
                                                )
import           Control.Monad.Reader           ( asks
                                                , ask
                                                , local
                                                , runReaderT
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Opal.Typing.Inference
import qualified Data.Map                      as M

-- Attempts to get a local from the current env, throws an error if it doesn't exist
getLocal :: Name -> TypeCheck Type
getLocal name = asks (get name . view locals) >>= \case
  Just t  -> pure t
  Nothing -> throwError (NotInScope name)


instance Checkable Lit Type where
  getType (LInt    _) = pure TInt
  getType (LString _) = pure TString
  getType (LChar   _) = pure TChar
  getType (LBool   _) = pure TBool
  getType (LFloat  _) = pure TFloat

-- Throws an error if the types don't match
unifyArgs :: [Type] -> [Type] -> TypeCheck ()
unifyArgs args params =
  mapM_
      (\(arg, param, i) -> if arg /= param
        then throwError
          (  Custom
          $  "Expected "
          <> show i
          <> "th argument to be of type "
          <> show param
          <> ", but found type "
          <> show arg
          )
        else pure ()
      )
    $ zip3 args params [1 ..]

expect :: Type -> Type -> TypeCheck ()
expect t1 t2 = if t1 == t2 then pure () else throwError (Mismatch t1 t2)

expectList :: Type -> [Type] -> TypeCheck ()
expectList _  []        = pure ()
expectList t1 (t2 : xs) = expect t1 t2 *> expectList t1 xs

instance Checkable Param Type where
  getType (Param (_, t)) = pure t

instance Checkable Expr Type where
  -- Function call
  getType (ECall name args) = do
    args <- traverse getType args
    let expected = TFn (TVar "a") args
    getLocal name >>= \case
      t@(TFn ret params) -> if length params /= length args
        then throwError
          (  Custom
          $  "Expected "
          <> show (length params)
          <> " arguments, but found "
          <> show (length args)
          )
        else unifyArgs args params $> ret
      t -> throwError (Mismatch expected t)

-- Literal
  getType (ELit  lit        ) = getType lit

-- List
  getType (EList []         ) = pure (TList (TVar "a"))
  getType (EList (e : exprs)) = do
    t1        <- getType e
    exprsType <- traverse getType exprs
    expectList t1 exprsType $> TList t1

-- Lambda
  getType (ELam params expr) = do
    params <- traverse getType params
    ret    <- getType expr
    pure (TFn ret params)

-- If condition
  getType (EIf cond e1 e2) = do
    condType <- getType cond
    expect TBool condType
    t1 <- getType e1
    t2 <- getType e2
    expect t1 t2
    pure t1

-- Variable
  getType (EVar   name ) = getLocal name

-- Block expr
  getType (EBlock stmts) = statements stmts

emptyTup :: Type
emptyTup = TTup []

statements :: [Stmt] -> TypeCheck Type
statements [] = pure emptyTup
statements stmts =
  snd
    <$> foldM (\(f, t) i -> (\(a, b) -> (f . a, b)) <$> local f (getType i))
              (id, emptyTup)
              stmts


instance Checkable Stmt (TypeCheckEnv -> TypeCheckEnv, Type) where
  getType (ExprStmt expr          ) = getType expr $> (id, emptyTup)
  getType (VarDeclStmt name t expr) = do
    tvar <- newTyVar "a"
    let t' = fromMaybe tvar t
    exprT <- local (supply +~ 1) (getType expr)
    subst <- unify t' exprT
    let newT = apply subst t'
    pure (locals %~ add name newT, emptyTup)
  getType (WhenStmt cond expr) = do
    condT <- getType cond
    expect TBool condT
    getType expr $> (id, emptyTup)
  getType (FnDeclStmt (FnDecl name params ret expr)) = do
-- Create a new type variable
    tVar <- newTyVar "a"
    let retType = fromMaybe tVar ret
    -- Get the types of the parameters
    paramTypes <- traverse getType params
    -- Function's type
    let fnType = TFn retType paramTypes
    -- Type check the returned expression
    t     <- local ((locals %~ add name fnType) . (supply +~ 1)) (getType expr)
    -- Unify t with the return type
    subst <- unify retType t
    -- Substitute the return type
    let newRetType = apply subst retType
    -- Set the new return type
    pure (locals %~ add name (TFn newRetType paramTypes), emptyTup)

instance Checkable Program () where
  getType (Program fns) = statements (map FnDeclStmt fns) $> ()

env :: TypeCheckEnv
env = TypeCheckEnv Nothing M.empty M.empty 0

typeCheck :: Program -> IO ()
typeCheck p = runExceptT (runReaderT (getType p) env) >>= \case
  Left  e -> print e
  Right _ -> print p
