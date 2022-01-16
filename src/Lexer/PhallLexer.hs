{-# LANGUAGE OverloadedStrings #-}

module Lexer.PhallLexer
  ( Parser,
    tokenizeIdentifier,
    tokenizeSignedFloat,
    tokenizeSignedInteger,
    tokenizeSymbol,
    tokenizeString,
    tokenizeChar,
    tokenizeKeyword,
    betweenParenthesis,
    spaceConsumer,
  )
where

import Common (Parser)
import qualified Control.Monad as Monad (guard)
import qualified Data.Functor as Functor (void)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text (pack)
import Lexer.Symbol
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
    apostrophe = tokenizeSymbol ApostropheSymbol

-- TODO: add ${literal} syntax
-- TODO: add escaping
tokenizeString :: Parser Text
tokenizeString =
  fmap Text.pack $ quotation *> Megaparsec.manyTill Lexer.charLiteral quotation
  where
    quotation = tokenizeSymbol QuotationSymbol

tokenizeSignedInteger :: Parser Integer
tokenizeSignedInteger = Lexer.signed spaceConsumer integer
  where
    integer = lexeme Lexer.decimal

tokenizeSignedFloat :: Parser Double
tokenizeSignedFloat = Lexer.signed spaceConsumer float
  where
    float = lexeme Lexer.float

tokenizeSymbol :: Symbol -> Parser ()
tokenizeSymbol = do
  Functor.void . symbol

tokenizeKeyword :: Keyword -> Parser ()
tokenizeKeyword keyword =
  lexeme $
    Char.string (name keyword)
      *> Megaparsec.notFollowedBy identifierNextCharacters

tokenizeIdentifier :: Parser Text
tokenizeIdentifier =
  lexeme $ do
    identifier <-
      fmap Text.pack $
        (:) <$> identifierFirstCharacters <*> Megaparsec.many identifierNextCharacters
    Monad.guard $ notElem identifier $ map name (values :: [Keyword])
    return identifier

identifierFirstCharacters :: Parser Char
identifierFirstCharacters = Char.letterChar <|> underscore

identifierNextCharacters :: Parser Char
identifierNextCharacters = Char.alphaNumChar <|> underscore

underscore :: Parser Char
underscore = Char.char '_'

betweenParenthesis :: Parser a -> Parser a
betweenParenthesis =
  Megaparsec.between (symbol OpenParenthesisSymbol) (symbol CloseParenthesisSymbol)

spaceConsumer :: Parser ()
spaceConsumer =
  Lexer.space
    Char.space1
    lineComment
    Megaparsec.empty
  where
    lineComment = Lexer.skipLineComment $ name LineCommentSymbol

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: (EnumValues a) => a -> Parser Text
symbol = Lexer.symbol spaceConsumer . name
