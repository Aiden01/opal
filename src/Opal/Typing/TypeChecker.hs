{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances #-}
module Opal.Typing.TypeChecker
  ()
where

import           Opal.Common
import           Control.Lens
import           Opal.Typing.Types
import           Opal.Parsing.AST
import           Control.Monad.Except           ( throwError )
import           Data.Functor                   ( ($>) )
import           Control.Monad                  ( (>=>)
                                                , foldM_
                                                )
import           Control.Monad.Reader           ( asks
                                                , local
                                                )
import           Data.Maybe                     ( fromMaybe )

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

-- Compares a list of arguments with a list of function parameters.
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

unify :: Type -> Type -> TypeCheck ()
unify t t' = if t == t' then pure () else throwError (Mismatch t t')

unifyList :: Checkable a Type => Type -> [a] -> TypeCheck ()
unifyList t = mapM_ (getType >=> unify t)

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
    t1 <- getType e
    unifyList t1 exprs $> TList t1

-- Lambda
  getType (ELam params expr) = do
    params <- traverse getType params
    ret    <- getType expr
    pure (TFn ret params)

-- If condition
  getType (EIf cond e1 e2) = do
    condType <- getType cond
    unify TBool condType
    t1 <- getType e1
    t2 <- getType e2
    unify t1 t2
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
  foldM_ (\f i -> (. f) <$> local f (fst <$> getType i)) id (init stmts)
    *> (snd <$> getType (last stmts))

instance Checkable Stmt (TypeCheckEnv -> TypeCheckEnv, Type) where
  getType (ExprStmt expr          ) = getType expr $> (id, emptyTup)
  getType (VarDeclStmt name t expr) = do
    exprT <- getType expr
    let t' = fromMaybe exprT t
    unify t' exprT
    pure (locals %~ add name t', emptyTup)
  getType (WhenStmt cond expr) = do
    condT <- getType cond
    unify TBool condT
    getType expr $> (id, emptyTup)
  getType (FnDeclStmt (FnDecl name params ret expr)) = do
    paramsT <- traverse getType params
    exprT   <- getType expr
    unify ret exprT
    let f = (tExpected ?~ ret) . (locals %~ add name (TFn ret paramsT))
    pure (f, emptyTup)

