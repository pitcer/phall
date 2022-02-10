{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallParser where

import Common
import Control.Applicative
import Control.Monad as Monad
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import FullSet
import Lexer.PhallLexer as Lexer
import Lexer.Symbol as Symbol
import Parser.PhallExpression as Expression
import Parser.PhallType as Type
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
  Megaparsec.try freestandingParser <|> Lexer.betweenParenthesis betweenParser

complexExpressions :: [Parser PhallExpression]
complexExpressions =
  [ Megaparsec.try parseImport,
    Megaparsec.try parseExport,
    Megaparsec.try parseDataDeclaration,
    Megaparsec.try parseLet,
    Megaparsec.try parseConditional,
    Megaparsec.try parseLambda,
    Megaparsec.try parseApplication
  ]

simpleExpressions :: [Parser PhallExpression]
simpleExpressions =
  [ Megaparsec.try parseDataInstance,
    Megaparsec.try parseList,
    ConstantExpression <$> parseConstant,
    parseIdentifier
  ]

parseLambda :: Parser PhallExpression
parseLambda = do
  parameters <- Megaparsec.some parseParameter
  Lexer.tokenizeSymbol RightArrowSymbol
  body <- parseExpression
  return $ foldr createLambda body parameters
  where
    createLambda (parameter, parameterType) previousLambda =
      LambdaExpression
        { parameter,
          maybeParameterType = parameterType,
          body = previousLambda,
          maybeBodyType = Nothing
        }

parseApplication :: Parser PhallExpression
parseApplication = do
  function <- betweenParenthesisOrNot parseIdentifier parseExpression
  arguments <- Megaparsec.some parseInnerExpression
  return $ foldl createApplication function arguments
  where
    createApplication previousApplication argument =
      ApplicationExpression {function = previousApplication, argument}

parseImport :: Parser PhallExpression
parseImport = do
  Lexer.tokenizeKeyword ImportKeyword
  importedItems <- parseImportedOrExportedItems
  Lexer.tokenizeKeyword FromKeyword
  importSource <- parsePath <|> parseInnerExpression
  Lexer.tokenizeKeyword InKeyword
  importBody <- parseExpression
  return ImportExpression {importSource, importedItems, importBody}

parsePath :: Parser PhallExpression
parsePath = do
  path <- Lexer.tokenizePath
  --  TODO: import from file on given path
  fail $ Text.unpack ("Tried to import a path: " <> path)

parseExport :: Parser PhallExpression
parseExport = do
  Lexer.tokenizeKeyword ExportKeyword
  ExportExpression <$> parseImportedOrExportedItems

parseImportedOrExportedItems :: Parser (FullSet Name)
parseImportedOrExportedItems =
  Megaparsec.choice
    [ Full <$ Lexer.tokenizeSymbol AsteriskSymbol,
      do
        items <- Megaparsec.some $ Megaparsec.try Lexer.tokenizeIdentifier
        let itemsSet = Set.fromList items
        Monad.guard $ length items == length itemsSet
        return $ NotFull itemsSet
    ]

parseDataDeclaration :: Parser PhallExpression
parseDataDeclaration = do
  Lexer.tokenizeKeyword DataKeyword
  name <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol EqualitySymbol
  fields <- Megaparsec.sepBy parseField $ Lexer.tokenizeSymbol CommaSymbol
  Lexer.tokenizeKeyword InKeyword
  body <- parseExpression
  return
    DataDeclarationExpression
      { declarationName = name,
        declarationFields = fields,
        declarationBody = body
      }

parseDataInstance :: Parser PhallExpression
parseDataInstance = do
  instanceName <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol LeftCurlyBracket
  instanceFields <- Megaparsec.sepBy parseFieldInstance $ Lexer.tokenizeSymbol CommaSymbol
  Lexer.tokenizeSymbol RightCurlyBracket
  return DataInstanceExpression {instanceName, instanceFields}

parseFieldInstance :: Parser DataInstanceField
parseFieldInstance = do
  fieldName <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol EqualitySymbol
  fieldValue <- parseExpression
  return DataInstanceField {Expression.fieldName, Expression.fieldValue}

parseField :: Parser DataTypeField
parseField = do
  fieldName <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol ColonSymbol
  typeName <- Lexer.tokenizeIdentifier -- TODO: replace with parseType
  let typeKeyword = Symbol.fromName typeName
  let maybeType = fmap Type.fromTypeKeyword typeKeyword
  case maybeType of
    Nothing -> fail "unknown type token"
    Just fieldType -> return DataTypeField {Type.fieldName, Type.fieldType}

parseLet :: Parser PhallExpression
parseLet = do
  Lexer.tokenizeKeyword LetKeyword
  (variableName, variableType) <- parseParameter
  maybeParameter <- Megaparsec.optional parseParameter
  Lexer.tokenizeSymbol EqualitySymbol
  value <- parseExpression
  Lexer.tokenizeKeyword InKeyword
  body <- parseExpression
  let function =
        LambdaExpression
          { parameter = variableName,
            maybeParameterType = variableType,
            body,
            maybeBodyType = Nothing
          }
  return
    ApplicationExpression
      { function,
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

parseParameter :: Parser (Name, Maybe PhallType)
parseParameter = do
  name <- Lexer.tokenizeIdentifier
  typeKeyword <- parseType
  return (name, fmap Type.fromTypeKeyword typeKeyword)

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
