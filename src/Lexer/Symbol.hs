{-# LANGUAGE OverloadedStrings #-}

module Lexer.Symbol
  ( EnumValues (..),
    Keyword (..),
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
  | LetKeyword
  | InKeyword
  | TrueKeyword
  | FalseKeyword
  deriving (Enum, Bounded)

instance EnumValues Keyword where
  name IfKeyword = "if"
  name ThenKeyword = "then"
  name ElseKeyword = "else"
  name LetKeyword = "let"
  name InKeyword = "in"
  name TrueKeyword = "true"
  name FalseKeyword = "false"

data Symbol
  = OpenParenthesisSymbol
  | CloseParenthesisSymbol
  | RightArrowSymbol
  | EqualitySymbol
  | QuotationSymbol
  | ApostropheSymbol
  | LineCommentSymbol
  deriving (Enum, Bounded)

instance EnumValues Symbol where
  name OpenParenthesisSymbol = "("
  name CloseParenthesisSymbol = ")"
  name RightArrowSymbol = "->"
  name EqualitySymbol = "="
  name QuotationSymbol = "\""
  name ApostropheSymbol = "'"
  name LineCommentSymbol = "#"
