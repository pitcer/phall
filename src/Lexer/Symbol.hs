{-# LANGUAGE OverloadedStrings #-}

module Lexer.Symbol where

import Data.Text.Lazy as Text

class (Enum a, Bounded a) => EnumValues a where
  enumName :: a -> Text

  enumValues :: [a]
  enumValues = [minBound .. maxBound]

data Keyword
  = IfKeyword
  | ThenKeyword
  | ElseKeyword
  | LetKeyword
  | InKeyword
  | DataKeyword
  | ImportKeyword
  | ExportKeyword
  | FromKeyword
  | TrueKeyword
  | FalseKeyword
  deriving (Enum, Bounded)

instance EnumValues Keyword where
  enumName IfKeyword = "if"
  enumName ThenKeyword = "then"
  enumName ElseKeyword = "else"
  enumName LetKeyword = "let"
  enumName InKeyword = "in"
  enumName DataKeyword = "data"
  enumName ImportKeyword = "import"
  enumName ExportKeyword = "export"
  enumName FromKeyword = "from"
  enumName TrueKeyword = "true"
  enumName FalseKeyword = "false"

data TypeKeyword
  = AnyTypeKeyword
  | BooleanTypeKeyword
  | IntegerTypeKeyword
  | FloatTypeKeyword
  | CharTypeKeyword
  | StringTypeKeyword
  deriving (Enum, Bounded)

instance EnumValues TypeKeyword where
  enumName AnyTypeKeyword = "Any"
  enumName BooleanTypeKeyword = "Boolean"
  enumName IntegerTypeKeyword = "Integer"
  enumName FloatTypeKeyword = "Float"
  enumName CharTypeKeyword = "Char"
  enumName StringTypeKeyword = "String"

data Symbol
  = LeftParenthesisSymbol
  | RightParenthesisSymbol
  | LeftSquareBracket
  | RightSquareBracket
  | LeftCurlyBracket
  | RightCurlyBracket
  | RightArrowSymbol
  | AsteriskSymbol
  | QuestionMark
  | EqualitySymbol
  | CommaSymbol
  | ColonSymbol
  | QuotationSymbol
  | ApostropheSymbol
  | GraveSymbol
  | LineCommentSymbol
  deriving (Enum, Bounded)

instance EnumValues Symbol where
  enumName LeftParenthesisSymbol = "("
  enumName RightParenthesisSymbol = ")"
  enumName LeftSquareBracket = "["
  enumName RightSquareBracket = "]"
  enumName LeftCurlyBracket = "{"
  enumName RightCurlyBracket = "}"
  enumName RightArrowSymbol = "->"
  enumName AsteriskSymbol = "*"
  enumName QuestionMark = "?"
  enumName EqualitySymbol = "="
  enumName CommaSymbol = ","
  enumName ColonSymbol = ":"
  enumName QuotationSymbol = "\""
  enumName ApostropheSymbol = "'"
  enumName GraveSymbol = "`"
  enumName LineCommentSymbol = "#"
