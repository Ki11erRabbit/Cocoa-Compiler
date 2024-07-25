{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, unpack)
import Data.Void
import Control.Monad.Combinators.Expr
import Ast
import Debug.Trace(trace)

type Parser = Parsec Void Text

skipParser :: Parser ()
skipParser = space 

parseExpr :: Parser Expr
parseExpr = parseOp <?> "expression"

term = parseSimpleExpr <|> parseOp <?> "term"

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

parseOp :: Parser Expr
parseOp = do
  trace "parseOp" $ return ()
  makeExprParser term table <?> "expression"

parseLiteral :: Parser Expr
parseLiteral = do
  trace "parseLiteral" $ return ()
  literal <- parseIntLiteral <|> parseFloatLiteral <|> parseCharLiteral <|> parseStringLiteral <|> parseBoolLiteral
  return $ Literal literal
  where
    parseIntLiteral = do
      int <- myinteger
      return $ IntLit int
    parseFloatLiteral = do
      float <- myfloat
      return $ FloatLit float
    parseCharLiteral = do
      char <- mychar
      return $ CharLit char
    parseStringLiteral = do
      string <- mystring
      return $ StringLit string
    parseBoolLiteral = do
      bool <- mybool
      return $ BoolLit bool


myinteger :: Parser String
myinteger = try $ do
  trace "myinteger" $ return ()
  out <- takeWhile1P (Just "integer") (\c -> c >= '0' && c <= '9')
  trace ("myinteger: " ++ show out) $ return ()
  _ <- skipParser
  return (unpack out)

myfloat :: Parser String
myfloat = do
  whole <- many digitChar
  char '.'
  decimal <- many digitChar
  return $ whole ++ "." ++ decimal

mychar :: Parser Char
mychar = between (char '\'') (char '\'') anyChar

mystring :: Parser String
mystring = do
  char '"'
  str <- manyTill anyChar (char '"')
  return str

mybool :: Parser Bool
mybool = do
  bool <- try (string "true") <|> string "false"
  return $ bool == "true"

anyChar :: Parser Char
anyChar = satisfy (const True)

parseVar :: Parser Expr
parseVar = do
  varName <- myidentifier
  return $ Var varName

parseSimpleExpr :: Parser Expr
parseSimpleExpr = try $ do
  trace "parseSimpleExpr" $ return ()
  expr <- parseLiteral <|> parseVar
  return expr

parseParenExpr :: Parser Expr
parseParenExpr = do
  _ <- char '('
  expr <- parseExpr
  _ <- char ')'
  return expr

myidentifier :: Parser String
myidentifier = do
  first <- letterChar <|> char '_'
  rest <- many alphaNumChar
  return $ first : rest
