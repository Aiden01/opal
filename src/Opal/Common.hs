{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Opal.Common
  ( Name
  , Env(..)
  , Scope
  )
where

import qualified Data.Map                      as M

type Name = String
type Scope = M.Map Name

class Env f where
  get :: Name -> f a -> Maybe a
  has :: Name -> f a -> Bool
  add :: Name -> a -> f a -> f a

instance Env Scope where
  get = M.lookup
  has = M.member
  add = M.insert
