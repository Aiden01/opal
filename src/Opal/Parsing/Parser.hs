module Opal.Parsing.Parser
  ( program
  , parseProgram
  )
where

import           Opal.Parsing.AST
import           Opal.Parsing.Lexer
import           Opal.Parsing.Types
import           Control.Applicative            ( (<|>) )
import           Text.Megaparsec                ( parse
                                                , eof
                                                , many
                                                , try
                                                , optional
                                                )
import           Text.Megaparsec.Error
import           Debug.Trace
import           Data.Functor                   ( ($>) )

lString, lInt, lFloat, lChar :: Parser Lit
lString = LString <$> stringLiteral
lInt = LInt <$> integerLiteral
lFloat = LFloat <$> floatLiteral
lChar = LChar <$> charLiteral

lit :: Parser Lit
lit = lChar <|> try lFloat <|> lString <|> lInt

eLiteral :: Parser Expr
eLiteral = ELit <$> lit

eLam :: Parser Expr
eLam = do
  symbol "λ" <|> symbol "\\"
  params <- parens (commaSep param)
  symbol "->"
  e <- try expr <|> EBlock <$> block
  pure (ELam params e)


param :: Parser Param
param = do
  name <- identifier
  t    <- symbol ":" *> typeAnn
  pure $ Param (name, t)

eList :: Parser Expr
eList = EList <$> brackets (commaSep expr)

eVar :: Parser Expr
eVar = EVar <$> identifier

typeAnn :: Parser Type
typeAnn =
  keyword "String"
    $>  TString
    <|> keyword "Float"
    $>  TFloat
    <|> keyword "Int"
    $>  TInt
    <|> keyword "Char"
    $>  TChar
    <|> TVar
    <$> identifier

eCall :: Parser Expr
eCall = do
  name   <- identifier
  params <- parens (commaSep expr)
  pure (ECall name params)

eIf :: Parser Expr
eIf = do
  keyword "if"
  e1 <- expr
  keyword "then"
  e2 <- expr
  keyword "else"
  e3 <- expr
  pure (EIf e1 e2 e3)

expr :: Parser Expr
expr =
  parens expr <|> eLiteral <|> eList <|> try eCall <|> eIf <|> eVar <|> eLam

program :: Parser Program
program = Program <$> (lexeme space *> many fnDecl)

block :: Parser Block
block = braces $ many (stmt <* semi)

stmt :: Parser Stmt
stmt = FnDeclStmt <$> fnDecl <|> exprStmt <|> varDecl

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr

fnDecl :: Parser FnDecl
fnDecl = do
  keyword "func"
  name <- identifier
  args <- parens (commaSep param)
  symbol "="
  body <- try expr <|> EBlock <$> block
  pure (FnDecl name args body)

varDecl :: Parser Stmt
varDecl = do
  keyword "let"
  name <- identifier
  t    <- optional (colon *> typeAnn)
  symbol "="
  e <- expr
  pure (VarDeclStmt name t e)



parseProgram :: String -> IO ()
parseProgram buffer = case parse (program <* eof) "" buffer of
  Left  e   -> putStrLn (errorBundlePretty e)
  Right ast -> print ast
