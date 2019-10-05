{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Opal.Typing.TypeChecker
  ()
where

import           Opal.Common
import           Control.Lens
import           Opal.Typing.Types
import           Opal.Parsing.AST
import           Control.Monad.Except           ( throwError )
import           Data.Functor                   ( ($>) )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Reader           ( asks )

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

instance Checkable Expr Type where
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

  getType (ELit  lit        ) = getType lit

  getType (EList []         ) = pure (TList (TVar "a"))
  getType (EList (e : exprs)) = do
    t1 <- getType e
    unifyList t1 exprs $> TList t1


