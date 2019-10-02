module Opal.Parsing.Common
  (
    block
  ) where

import Opal.Parsing.AST
import Opal.Parsing.Program

eLam :: Parser Expr
eLam = do
  
