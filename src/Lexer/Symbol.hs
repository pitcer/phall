{-# LANGUAGE OverloadedStrings #-}

module Lexer.Symbol
  ( EnumValues (..),
    Keyword (..),
    Operator (..),
    Symbol (..),
  )
where

import Data.Text.Lazy (Text)

class (Enum a, Bounded a) => EnumValues a where
  name :: a -> Text

  values :: [a]
  values = [minBound .. maxBound]

data Keyword
  = IfKeyword
  | ThenKeyword
  | ElseKeyword
  | TrueKeyword
  | FalseKeyword
  deriving (Enum, Bounded)

instance EnumValues Keyword where
  name IfKeyword = "if"
  name ThenKeyword = "then"
  name ElseKeyword = "else"
  name TrueKeyword = "true"
  name FalseKeyword = "false"

data Operator
  = RightArrowOperator
  | LambdaOperator
  deriving (Enum, Bounded)

instance EnumValues Operator where
  name RightArrowOperator = "->"
  name LambdaOperator = ".\\"

data Symbol
  = OpenParenthesis
  | CloseParenthesis
  | Quotation
  | Apostrophe
  | LineComment
  deriving (Enum, Bounded)

instance EnumValues Symbol where
  name OpenParenthesis = "("
  name CloseParenthesis = ")"
  name Quotation = "\""
  name Apostrophe = "'"
  name LineComment = "#"
