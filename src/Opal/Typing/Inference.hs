module Opal.Typing.Inference
  ( Types(..)
  , Subst
  , unify
  , newTyVar
  )
where

import qualified Data.Set                      as S
import           Opal.Common
import           Opal.Parsing.AST               ( Type(..) )
import           Opal.Typing.Types
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Control.Lens
import           Control.Monad                  ( foldM )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.Reader           ( asks )

class Types a where
  -- Get the list of all type variables
  ftv :: a -> S.Set Name
  -- Substitute a type variable with a concrete type
  apply :: Subst -> a -> a

-- All substitutions
type Subst = Scope Type

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv = foldr (S.union . ftv) S.empty

instance Types Type where
  ftv (TVar n)        = S.singleton n
  ftv TInt            = S.empty
  ftv TBool           = S.empty
  ftv TChar           = S.empty
  ftv TString         = S.empty
  ftv (TList t1     ) = ftv t1
  ftv (TFn t1 params) = ftv t1 `S.union` ftv params
  ftv (TTup types   ) = ftv types

  apply s (TVar n       ) = fromMaybe (TVar n) (get n s)
  apply s (TFn t1 params) = TFn (apply s t1) (apply s params)
  apply s (TList t1     ) = apply s t1
  apply s (TTup  types  ) = TTup (map (apply s) types)
  apply s t               = t

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (apply s1) s2 `M.union` s1

instance Types TypeCheckEnv where
  ftv = ftv . elems . view locals
  apply s = locals %~ M.map (apply s)

unifyList :: Subst -> [Type] -> [Type] -> TypeCheck Subst
unifyList s1 = foldM (\s -> (composeSubst s <$>) . uncurry unify) s1 .: zip

unify :: Type -> Type -> TypeCheck Subst
unify (TFn ret args) (TFn ret' args') = do
  s1 <- unify ret ret'
  s2 <- unifyList s1 (apply s1 args) (apply s1 args')
  pure (s1 `composeSubst` s2)
unify (TVar u)  t         = varBind u t
unify t         (TVar u)  = varBind u t
unify TInt      TInt      = pure M.empty
unify TBool     TBool     = pure M.empty
unify TString   TString   = pure M.empty
unify TChar     TChar     = pure M.empty
unify (TTup t1) (TTup t2) = unifyList M.empty t1 t2
unify (TList a) (TList b) = unify a b
unify t1        t2        = throwError (Mismatch t1 t2)

varBind :: String -> Type -> TypeCheck Subst
varBind u t
  | t == TVar u = pure M.empty
  | u `S.member` ftv t = throwError
  $ Custom ("occur check fails: " <> u <> " vs. " <> show t)
  | otherwise = pure (M.singleton u t)

newTyVar :: String -> TypeCheck Type
newTyVar prefix = do
  suffix <- (+ 1) <$> asks (view supply)
  pure $ TVar (prefix ++ show suffix)




