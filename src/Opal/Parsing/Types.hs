module Opal.Parsing.Types
  ( Parser 
  )
where

import Text.Megaparsec (Parsec)
import Data.Void (Void)

type Parser = Parsec Void String
