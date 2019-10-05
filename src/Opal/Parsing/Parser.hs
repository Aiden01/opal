module Opal.Parsing.Parser
  ( program
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
import           Data.Functor                   ( ($>) )
import           Control.Monad.Combinators.Expr

lString, lInt, lFloat, lChar :: Parser Lit
lString = LString <$> stringLiteral
lBool = (keyword "true" $> LBool True) <|> (keyword "false" $> LBool False)
lInt = LInt <$> integerLiteral
lFloat = LFloat <$> floatLiteral
lChar = LChar <$> charLiteral

lit :: Parser Lit
lit = lChar <|> try lFloat <|> lString <|> lInt <|> lBool

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

-- operators
binOP :: String -> BinOP -> Parser (Expr -> Expr -> Expr)
binOP xs op = operator xs $> EBinOP op

unOP :: String -> UnOP -> Parser (Expr -> Expr)
unOP xs op = operator xs $> EUnOP op

opTable =
  [ [InfixL (binOP "&&" BAnd)]
  , [InfixL (binOP "||" BOR)]
  , [InfixL (binOP "==" BEQ)]
  , [InfixL (binOP "!=" BNotEQ)]
  , [InfixL (binOP ">" BGT)]
  , [InfixL (binOP ">=" BGTE)]
  , [InfixL (binOP "<" BLT)]
  , [InfixL (binOP "=<" BLTE)]
  , [InfixL (binOP "**" BPow)]
  , [InfixL (binOP "/" BDiv)]
  , [InfixL (binOP "*" BMult)]
  , [InfixL (binOP "+" BAdd)]
  , [InfixL (binOP "-" BSub)]
  -- unary operators
  , [Prefix (unOP "!" UNot)]
  , [Postfix (unOP "++" UInc)]
  , [Postfix (unOP "--" UInc)]
  ]


expr :: Parser Expr
expr = makeExprParser term opTable
 where
  term =
    parens expr <|> eLiteral <|> eList <|> try eCall <|> eIf <|> eVar <|> eLam

-- statements

program :: Parser Program
program = Program <$> (lexeme space *> many fnDecl)

block :: Parser Block
block = braces $ many (stmt <* semi)

stmt :: Parser Stmt
stmt = FnDeclStmt <$> fnDecl <|> exprStmt <|> varDecl <|> whenStmt <|> retStmt

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr

fnDecl :: Parser FnDecl
fnDecl = do
  keyword "func"
  name <- identifier
  args <- parens (commaSep param)
  ret  <- colon *> typeAnn
  symbol "="
  body <- try expr <|> EBlock <$> block
  pure (FnDecl name args ret body)

varDecl :: Parser Stmt
varDecl = do
  keyword "let"
  name <- identifier
  t    <- optional (colon *> typeAnn)
  symbol "="
  e <- expr
  pure (VarDeclStmt name t e)

whenStmt :: Parser Stmt
whenStmt = do
  keyword "when"
  cond <- parens expr
  symbol "="
  body <- try expr <|> EBlock <$> block
  pure (WhenStmt cond body)

retStmt :: Parser Stmt
retStmt = RetStmt <$> (keyword "return" *> expr)
