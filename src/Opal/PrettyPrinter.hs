{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Lispy.PrettyPrinter
(prettyPrint)
where

import           Text.PrettyPrint.ANSI.Leijen   ( Pretty
                                                , pretty
                                                , integer
                                                , (<>)
                                                , (<+>)
                                                , Doc
                                                , text
                                                , double
                                                , char
                                                , bool
                                                , linebreak
                                                , indent
                                                , vsep
                                                , hsep
                                                )
import Lispy.Parsing.AST
import Control.Comonad.Cofree (Cofree((:<)))

colon :: String -> Doc
colon s = text (s <> ": ")

instance Pretty Lit where
  pretty (LInt x) = colon "Int" <> integer x
  pretty (LFloat x) = colon "Float" <> double x
  pretty (LString x) = colon "String" <> text x
  pretty (LChar x) = colon "Char" <> char x


instance Pretty Expr where
  pretty (_ :< e) = pretty e

instance Pretty (ExprF Expr) where
  pretty (ELit lit) = pretty lit

prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty
