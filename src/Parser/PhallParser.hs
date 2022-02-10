{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallParser where

import Common
import Control.Applicative
import Control.Monad as Monad
import Data.Maybe as Maybe
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
    parseVariable
  ]

parseLambda :: Parser PhallExpression
parseLambda = do
  parameters <- Megaparsec.some parseParameter
  Lexer.tokenizeSymbol RightArrowSymbol
  body <- parseExpression
  return $ desugarMultiparameterLambda body parameters

parseApplication :: Parser PhallExpression
parseApplication = do
  function <- betweenParenthesisOrNot parseVariable parseExpression
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
  fieldType <- parseType
  return DataTypeField {Type.fieldName, Type.fieldType}

parseLet :: Parser PhallExpression
parseLet = do
  Lexer.tokenizeKeyword LetKeyword
  variable <- parseParameter
  maybeParameter <- Megaparsec.optional $ Megaparsec.some parseParameter
  Lexer.tokenizeSymbol EqualitySymbol
  value <- parseExpression
  Lexer.tokenizeKeyword InKeyword
  body <- parseExpression
  let function =
        LambdaExpression
          { parameter = variable,
            body,
            Expression.bodyType = UnknownType
          }
  return
    ApplicationExpression
      { function,
        argument = desugarFunction value maybeParameter
      }
  where
    desugarFunction value Nothing = value
    desugarFunction body (Just parameters) =
      desugarMultiparameterLambda body parameters

parseParameter :: Parser LambdaParameter
parseParameter = do
  parameterName <- Lexer.tokenizeIdentifier
  parameterType <-
    Maybe.fromMaybe UnknownType
      <$> Megaparsec.optional (Lexer.tokenizeSymbol ColonSymbol *> parseType)
  return LambdaParameter {parameterName, Expression.parameterType}

desugarMultiparameterLambda :: PhallExpression -> [LambdaParameter] -> PhallExpression
desugarMultiparameterLambda = foldr createLambda
  where
    createLambda parameter previousLambda =
      LambdaExpression
        { parameter,
          body = previousLambda,
          Expression.bodyType = UnknownType
        }

parseType :: Parser PhallType
parseType = do
  betweenParenthesisOrNot innerParser innerParser
  where
    innerParser = Megaparsec.choice $ complexTypes ++ simpleTypes

parseInnerType :: Parser PhallType
parseInnerType =
  betweenParenthesisOrNot
    (Megaparsec.choice simpleTypes)
    (Megaparsec.choice complexTypes)

simpleTypes :: [Parser PhallType]
simpleTypes =
  [parseListType] ++ parseTypeKeywords ++ [parseNamedType]
  where
    parseListType = Megaparsec.try $ do
      Lexer.tokenizeSymbol LeftSquareBracket
      listType <- parseType
      Lexer.tokenizeSymbol RightSquareBracket
      return $ ListType listType

    parseTypeKeywords =
      map (Megaparsec.try . fmap Type.fromTypeKeyword . Lexer.symbol) enumValues

    parseNamedType =
      Megaparsec.try $ NamedType <$> Lexer.tokenizeIdentifier

complexTypes :: [Parser PhallType]
complexTypes =
  [parseLambdaType]
  where
    parseLambdaType = Megaparsec.try $ do
      parameterType <- parseInnerType
      Lexer.tokenizeSymbol RightArrowSymbol
      bodyType <- parseType
      return $ LambdaType {Type.parameterType, Type.bodyType}

betweenParenthesisOrNot :: Parser a -> Parser a -> Parser a
betweenParenthesisOrNot freestandingParser betweenParser =
  Megaparsec.try freestandingParser <|> Lexer.betweenParenthesis betweenParser

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

parseVariable :: Parser PhallExpression
parseVariable =
  VariableExpression <$> Lexer.tokenizeIdentifier
