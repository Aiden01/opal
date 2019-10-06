{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Opal.Common
  ( Name
  , Env(..)
  , Scope
  , (.:)
  )
where

import qualified Data.Map                      as M

type Name = String
type Scope = M.Map Name

class Env f where
  get :: Name -> f a -> Maybe a
  has :: Name -> f a -> Bool
  add :: Name -> a -> f a -> f a
  elems :: f a -> [a]

instance Env Scope where
  get   = M.lookup
  has   = M.member
  add   = M.insert
  elems = M.elems


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x y -> f (g x y)
