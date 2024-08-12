module Ast.Shared  where

import Text.Megaparsec.Pos (SourcePos)

data Span = Span SourcePos SourcePos
  deriving (Show, Eq)

data Spanned a = Spanned Span a
  deriving (Show, Eq)
