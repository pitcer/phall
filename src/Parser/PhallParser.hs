{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallParser
  ( PhallExpression (..),
    PhallConstant (..),
    parse,
    VariableName,
  )
where

import Lexer.PhallLexer as Lexer
import Lexer.Symbol as Symbol
import Parser.PhallExpression
import Parser.PhallType as PhallType
import qualified Text.Megaparsec as Megaparsec

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
  parameters <- Megaparsec.some parseParameter
  Lexer.tokenizeSymbol RightArrowSymbol
  body <- parseExpression
  return $
    foldr
      ( \(parameter, parameterType) previousLambdas ->
          LambdaExpression
            { parameter,
              maybeParameterType = parameterType,
              body = previousLambdas,
              maybeBodyType = Nothing
            }
      )
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
  (variableName, variableType) <- parseParameter
  maybeParameter <- Megaparsec.optional parseParameter
  Lexer.tokenizeSymbol EqualitySymbol
  value <- parseExpression
  Lexer.tokenizeKeyword InKeyword
  body <- parseExpression
  return
    ApplicationExpression
      { function =
          LambdaExpression
            { parameter = variableName,
              maybeParameterType = variableType,
              body,
              maybeBodyType = Nothing
            },
        argument = desugarFunction value maybeParameter
      }
  where
    desugarFunction value Nothing = value
    desugarFunction body (Just (parameter, parameterType)) =
      LambdaExpression
        { parameter,
          maybeParameterType = parameterType,
          body,
          maybeBodyType = Nothing
        }

parseParameter :: Parser (VariableName, Maybe PhallType)
parseParameter = do
  name <- Lexer.tokenizeIdentifier
  typeKeyword <- parseType
  return (name, fmap PhallType.fromTypeKeyword typeKeyword)

parseType :: Parser (Maybe TypeKeyword)
parseType = do
  typeName <- Megaparsec.optional $ Lexer.tokenizeSymbol ColonSymbol *> Lexer.tokenizeIdentifier
  return $ typeName >>= Symbol.fromName

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
