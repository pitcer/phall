{-# LANGUAGE OverloadedStrings #-}

module PhallParser
  ( PhallExpression (..),
    PhallConstant (..),
    parse,
  )
where

import Common (Parser)
import qualified PhallLexer as Lexer
import Text.Megaparsec as Megaparsec (between, choice, eof, try)

data PhallExpression
  = ConstantExpression PhallConstant
  | VariableExpression String
  deriving (Show)

data PhallConstant
  = BooleanConstant Bool
  | IntegerConstant Integer
  | FloatConstant Double
  | CharConstant Char
  | StringConstant String
  deriving (Show)

parse :: Parser PhallExpression
parse = Megaparsec.between Lexer.spaceConsumer Megaparsec.eof parseExpression

parseExpression :: Parser PhallExpression
parseExpression =
  Megaparsec.choice
    [ ConstantExpression <$> parseConstant,
      VariableExpression <$> Lexer.tokenizeIdentifier
    ]

parseConstant :: Parser PhallConstant
parseConstant =
  Megaparsec.choice $ map Megaparsec.try constants
  where
    constants =
      [ BooleanConstant <$> parseBoolean,
        FloatConstant <$> Lexer.tokenizeSignedFloat,
        IntegerConstant <$> Lexer.tokenizeSignedInteger,
        CharConstant <$> Lexer.tokenizeChar,
        StringConstant <$> Lexer.tokenizeString
      ]

parseBoolean :: Parser Bool
parseBoolean =
  Megaparsec.choice
    [ True <$ Lexer.tokenizeKeyword "true",
      False <$ Lexer.tokenizeKeyword "false"
    ]
