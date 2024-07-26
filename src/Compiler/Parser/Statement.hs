{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Statement where


import Compiler.Ast.Statement
import Compiler.Parser.Shared
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr


parseExpr :: Parser Expr
parseExpr = parseBaseExpr <|> (parseOp parseExpr) <|>
  parseArrayExpr <|> parseArrayAccessExpr <|> parseFieldAccessExpr <|> parseParenExpr <|>
  parseCallExpr <|> parseCastExpr <|> parseInstanceofExpr <|> parseNewExpr <?> "Parse Expr"


parseNewExpr :: Parser Expr
parseNewExpr = do
  _ <- string "new"
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  _ <- char '('
  _ <- skipParser
  args <- sepBy parseExpr (mylexeme $ char ',')
  _ <- skipParser
  return $ NewExpr t args


parseArrayExpr :: Parser Expr
parseArrayExpr = do
  _ <- char '['
  _ <- skipParser
  exprs <- sepBy parseExpr (mylexeme $ char ',')
  _ <- skipParser
  _ <- char ']'
  _ <- skipParser
  return $ ArrayLiteral exprs

parseArrayAccessExpr :: Parser Expr
parseArrayAccessExpr = do
  expr <- parseExpr
  _ <- skipParser
  brackedExprs <- many bracketedExpr
  _ <- skipParser
  foldl ArrayAccess <$> return expr <*> return brackedExprs
  where
    bracketedExpr = do
      _ <- char '['
      _ <- skipParser
      e <- parseExpr
      _ <- skipParser
      _ <- char ']'
      _ <- skipParser
      return e

parseFieldAccessExpr :: Parser Expr
parseFieldAccessExpr = do
  expr <- parseExpr
  _ <- skipParser
  accessors <- many fieldAccessor
  _ <- skipParser
  return $ foldl FieldAccess expr accessors
  where
    fieldAccessor = do
      _ <- char '.'
      name <- myidentifier
      _ <- skipParser
      return name

parseParenExpr :: Parser Expr
parseParenExpr = do
  _ <- char '('
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  _ <- char ')'
  _ <- skipParser
  return $ Paren expr


parseCallExpr :: Parser Expr
parseCallExpr = do
  expr <- parseExpr
  _ <- skipParser
  args <- many calls
  _ <- skipParser
  foldl Call <$> return expr <*> return args
  where
    calls = do
      _ <- char '('
      _ <- skipParser
      args <- sepBy parseExpr (mylexeme $ char ',')
      _ <- skipParser
      _ <- char ')'
      _ <- skipParser
      return args

parseCastExpr :: Parser Expr
parseCastExpr = do
  expr <- parseExpr
  _ <- skipParser
  _ <- string "as"
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  return $ Cast t expr

parseInstanceofExpr :: Parser Expr
parseInstanceofExpr = do
  expr <- parseExpr
  _ <- skipParser
  _ <- string "instanceof"
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  return $ InstanceOf expr t



table = [ [prefix "!" (UnaryOp NotOp), prefix "-" (UnaryOp Neg)]
        , [binary "*" (BinaryOp Mul), binary "/" (BinaryOp Div), binary "%" (BinaryOp Mod)]
        , [binary "+" (BinaryOp Add), binary "-" (BinaryOp Sub)]
        , [binary "<<" (BinaryOp LShift), binary ">>" (BinaryOp RShift)]
        , [binary "<" (BinaryOp Lt), binary ">" (BinaryOp Gt), binary "<=" (BinaryOp Le), binary ">=" (BinaryOp Ge)]
        , [binary "==" (BinaryOp Eq), binary "!=" (BinaryOp Neq)]
        , [binary "&" (BinaryOp BitAnd)]
        , [binary "^" (BinaryOp BitXor)]
        , [binary "|" (BinaryOp BitOr)]
        , [binary "&&" (BinaryOp LogicalAnd)]
        , [binary "||" (BinaryOp LogicalOr)]
        , [binary ".." (BinaryOp ExclusiveRangeOp), binary "..=" (BinaryOp InclusiveRangeOp)]
        ]

binary name f = InfixL (do _ <- string name; _ <- skipParser; return f)
prefix name f = Prefix (do _ <- string name; _ <- skipParser; return f)

parseOp :: Parser Expr -> Parser Expr
parseOp term = do
  makeExprParser (term <|> (parseOp term) <?> "term") table <?> "expression"



 
parseClassLiteralsExpr :: Parser Expr
parseClassLiteralsExpr = parseThisLiteral <|> parseSuperLiteral <|> parseNullLiteral
  where parseThisLiteral = do
          _ <- string "this"
          return ThisExpr
        parseSuperLiteral = do
          _ <- string "super"
          return SuperExpr
        parseNullLiteral = do
          _ <- string "null"
          return NullExpr

parseLiteral :: Parser Expr
parseLiteral = do
  literal <- parseFloatLiteral <|> parseIntLiteral <|> parseCharLiteral <|> parseStringLiteral <|> parseBoolLiteral
  return $ Literal literal
  where
    parseIntLiteral = do
      int <- myinteger
      return $ IntLit int
    parseFloatLiteral = do
      float <- myfloat
      return $ FloatLit float
    parseCharLiteral = do
      charLit <- mychar
      return $ CharLit charLit
    parseStringLiteral = do
      stringLit <- mystring
      return $ StringLit stringLit
    parseBoolLiteral = do
      bool <- mybool
      return $ BoolLit bool


parseVar :: Parser Expr
parseVar = try $ do
  varName <- myidentifier
  return $ Var varName

parseBaseExpr :: Parser Expr
parseBaseExpr = try $ do
  expr <- parseLiteral <|> parseVar <|> parseClassLiteralsExpr <?> "Parse Base Expr"
  return expr
