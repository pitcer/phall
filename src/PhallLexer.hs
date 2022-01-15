{-# LANGUAGE OverloadedStrings #-}

module PhallLexer
  ( Parser,
    tokenizeIdentifier,
    tokenizeSignedFloat,
    tokenizeSignedInteger,
    tokenizeString,
    tokenizeChar,
    tokenizeKeyword,
    spaceConsumer,
  )
where

import Common (Parser)
import Data.Text (Text)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Megaparsec (between, empty, many, manyTill, notFollowedBy)
import qualified Text.Megaparsec.Char as Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer
  ( charLiteral,
    decimal,
    float,
    lexeme,
    signed,
    skipLineComment,
    space,
    symbol,
  )

tokenizeChar :: Parser Char
tokenizeChar =
  Megaparsec.between apostrophe apostrophe Lexer.charLiteral
  where
    apostrophe = symbol "\'"

-- TODO: add ${literal} syntax
tokenizeString :: Parser String
tokenizeString =
  quotation *> Megaparsec.manyTill Lexer.charLiteral quotation
  where
    quotation = symbol "\""

tokenizeSignedInteger :: Parser Integer
tokenizeSignedInteger = Lexer.signed spaceConsumer integer
  where
    integer = lexeme Lexer.decimal

tokenizeSignedFloat :: Parser Double
tokenizeSignedFloat = Lexer.signed spaceConsumer float
  where
    float = lexeme Lexer.float

-- TODO: check if it is a valid keyword
tokenizeKeyword :: Text -> Parser ()
tokenizeKeyword keyword =
  Char.string keyword *> Megaparsec.notFollowedBy identifierNextCharacters *> spaceConsumer

tokenizeIdentifier :: Parser String
tokenizeIdentifier =
  lexeme $ fmap (:) identifierFirstCharacters <*> Megaparsec.many identifierNextCharacters

identifierFirstCharacters :: Parser Char
identifierFirstCharacters = Char.letterChar <|> underscore

identifierNextCharacters :: Parser Char
identifierNextCharacters = Char.alphaNumChar <|> underscore

underscore :: Parser Char
underscore = Char.char '_'

spaceConsumer :: Parser ()
spaceConsumer =
  Lexer.space
    Char.space1
    lineComment
    Megaparsec.empty
  where
    lineComment = Lexer.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer
