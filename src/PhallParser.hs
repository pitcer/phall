{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PhallParser
  ( PhallExpression (..),
    PhallConstant (..),
    parse,
    VariableName,
  )
where

import Data.Text.Lazy (Text)
import Lexer.PhallLexer as Lexer
import Lexer.Symbol
import qualified Text.Megaparsec as Megaparsec

data PhallExpression
  = LambdaExpression
      { parameter :: VariableName,
        body :: PhallExpression
      }
  | ApplicationExpression
      { function :: PhallExpression,
        argument :: PhallExpression
      }
  | ConditionalExpression
      { condition :: PhallExpression,
        positive :: PhallExpression,
        negative :: PhallExpression
      }
  | ListExpression [PhallExpression]
  | ConstantExpression PhallConstant
  | VariableExpression VariableName
  deriving (Show, Eq)

type VariableName = Text

data PhallConstant
  = BooleanConstant Bool
  | IntegerConstant Integer
  | FloatConstant Double
  | CharConstant Char
  | StringConstant Text
  deriving (Show, Eq)

parse :: Parser PhallExpression
parse = Megaparsec.between Lexer.spaceConsumer Megaparsec.eof parseExpression

parseExpression :: Parser PhallExpression
parseExpression =
  betweenParenthesisOrNot innerParser innerParser
  where
    innerParser = Megaparsec.choice $ complexExpressions ++ simpleExpressions

parseInnerExpression :: Parser PhallExpression
parseInnerExpression =
  betweenParenthesisOrNot
    (Megaparsec.choice simpleExpressions)
    (Megaparsec.choice complexExpressions)

betweenParenthesisOrNot ::
  Parser PhallExpression -> Parser PhallExpression -> Parser PhallExpression
betweenParenthesisOrNot freestandingParser betweenParser =
  Megaparsec.choice [Megaparsec.try freestandingParser, Lexer.betweenParenthesis betweenParser]

complexExpressions :: [Parser PhallExpression]
complexExpressions =
  [ Megaparsec.try parseLet,
    Megaparsec.try parseConditional,
    Megaparsec.try parseLambda,
    Megaparsec.try parseApplication
  ]

simpleExpressions :: [Parser PhallExpression]
simpleExpressions =
  [ Megaparsec.try parseList,
    ConstantExpression <$> parseConstant,
    parseIdentifier
  ]

parseLambda :: Parser PhallExpression
parseLambda = do
  parameters <- Megaparsec.some Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol RightArrowSymbol
  body <- parseExpression
  return $
    foldr
      (\parameter previousLambdas -> LambdaExpression {parameter, body = previousLambdas})
      body
      parameters

parseApplication :: Parser PhallExpression
parseApplication = do
  function <- betweenParenthesisOrNot parseIdentifier parseExpression
  arguments <- Megaparsec.some parseInnerExpression
  return $
    foldl
      ( \previousApplications argument ->
          ApplicationExpression {function = previousApplications, argument}
      )
      function
      arguments

parseLet :: Parser PhallExpression
parseLet = do
  Lexer.tokenizeKeyword LetKeyword
  variableName <- Lexer.tokenizeIdentifier
  maybeParameter <- Megaparsec.optional Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol EqualitySymbol
  value <- parseExpression
  Lexer.tokenizeKeyword InKeyword
  body <- parseExpression
  return
    ApplicationExpression
      { function = LambdaExpression {parameter = variableName, body},
        argument = desugarFunction value maybeParameter
      }
  where
    desugarFunction value Nothing = value
    desugarFunction body (Just parameter) = LambdaExpression {parameter, body}

parseConditional :: Parser PhallExpression
parseConditional = do
  Lexer.tokenizeKeyword IfKeyword
  condition <- parseExpression
  Lexer.tokenizeKeyword ThenKeyword
  positive <- parseExpression
  Lexer.tokenizeKeyword ElseKeyword
  negative <- parseExpression
  return ConditionalExpression {condition, positive, negative}

parseList :: Parser PhallExpression
parseList = do
  Lexer.tokenizeSymbol LeftSquareBracket
  list <- Megaparsec.sepBy parseExpression $ Lexer.tokenizeSymbol CommaSymbol
  Lexer.tokenizeSymbol RightSquareBracket
  return $ ListExpression list

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
    [ True <$ Lexer.tokenizeKeyword TrueKeyword,
      False <$ Lexer.tokenizeKeyword FalseKeyword
    ]

parseIdentifier :: Parser PhallExpression
parseIdentifier =
  VariableExpression <$> Lexer.tokenizeIdentifier
