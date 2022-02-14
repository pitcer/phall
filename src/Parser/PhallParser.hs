{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallParser where

import Common
import Control.Applicative
import Control.Monad as Monad
import Control.Monad.Trans as Monad
import Data.Either as Either
import Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.IO as TextIO
import FullSet
import Lexer.PhallLexer as Lexer
import Lexer.Symbol as Symbol
import Parser.CommonParser as Parser
import Parser.PhallExpression as Expression
import Parser.PhallType as Type
import Parser.TypeParser as Parser
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
  map
    Megaparsec.try
    [ parseImport,
      parseExport,
      parseTypeDeclaration,
      parseDataDeclaration,
      parseLet,
      parseConditional,
      parseInternalCall,
      parseLambda,
      parseApplication
    ]

simpleExpressions :: [Parser PhallExpression]
simpleExpressions =
  [ Megaparsec.try parseDataInstance,
    Megaparsec.try parseTuple,
    Megaparsec.try parseList,
    ConstantExpression <$> parseConstant,
    parseVariable
  ]

parseLambda :: Parser PhallExpression
parseLambda = do
  parameters <- Megaparsec.some $ parseOptionalComma parseParameter parseParameter
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

parseInternalCall :: Parser PhallExpression
parseInternalCall = do
  calleeName <- Lexer.tokenizeSymbol AtSymbol *> Lexer.tokenizeIdentifier
  arguments <- Megaparsec.many parseInnerExpression
  return InternalCallExpression {calleeName, arguments}

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
  path <- Text.unpack <$> Lexer.tokenizePath
  pathSource <- Monad.liftIO . TextIO.readFile $ path
  pathExpression <- Monad.lift $ Megaparsec.runParserT parse path pathSource
  Either.either (Monad.fail . Megaparsec.errorBundlePretty) Monad.return pathExpression

parseExport :: Parser PhallExpression
parseExport = do
  Lexer.tokenizeKeyword ExportKeyword
  ExportExpression <$> parseImportedOrExportedItems

parseImportedOrExportedItems :: Parser (FullSet Name)
parseImportedOrExportedItems =
  Megaparsec.choice
    [ Full <$ Lexer.tokenizeSymbol AsteriskSymbol,
      do
        items <-
          Megaparsec.some $
            parseOptionalComma Lexer.tokenizeIdentifier Lexer.tokenizeIdentifier
        let itemsSet = Set.fromList items
        Monad.guard $ length items == length itemsSet
        return $ NotFull itemsSet
    ]

parseTypeDeclaration :: Parser PhallExpression
parseTypeDeclaration = do
  Lexer.tokenizeKeyword TypeKeyword
  typeDeclarationName <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol EqualitySymbol
  typeDeclarationType <- parseType
  Lexer.tokenizeKeyword InKeyword
  typeDeclarationBody <- parseExpression
  return TypeDeclarationExpression {typeDeclarationName, typeDeclarationType, typeDeclarationBody}

parseDataDeclaration :: Parser PhallExpression
parseDataDeclaration = do
  Lexer.tokenizeKeyword DataKeyword
  declarationName <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol EqualitySymbol
  declarationFields <- Megaparsec.some $ parseOptionalComma parseField parseField
  Lexer.tokenizeKeyword InKeyword
  declarationBody <- parseExpression
  return DataDeclarationExpression {declarationName, declarationFields, declarationBody}

parseDataInstance :: Parser PhallExpression
parseDataInstance = do
  instanceName <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol LeftCurlyBracket
  instanceFields <-
    Megaparsec.some $
      parseOptionalComma
        (parseFieldInstance parseInnerExpression)
        (parseFieldInstance parseExpression)
  Lexer.tokenizeSymbol RightCurlyBracket
  return DataInstanceExpression {instanceName, instanceFields}

parseFieldInstance :: Parser PhallExpression -> Parser DataInstanceField
parseFieldInstance valueParser = do
  fieldName <- Lexer.tokenizeIdentifier
  Lexer.tokenizeSymbol EqualitySymbol
  fieldValue <- valueParser
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
  parameters <- Megaparsec.many $ parseOptionalComma parseParameter parseParameter
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
        argument = desugarMultiparameterLambda value parameters
      }

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

parseConditional :: Parser PhallExpression
parseConditional = do
  Lexer.tokenizeKeyword IfKeyword
  condition <- parseExpression
  Lexer.tokenizeKeyword ThenKeyword
  positive <- parseExpression
  Lexer.tokenizeKeyword ElseKeyword
  negative <- parseExpression
  return ConditionalExpression {condition, positive, negative}

parseTuple :: Parser PhallExpression
parseTuple = do
  Lexer.tokenizeSymbol LeftCurlyBracket
  tuple <- Megaparsec.some $ parseOptionalComma parseInnerExpression parseExpression
  Lexer.tokenizeSymbol RightCurlyBracket
  return $ TupleExpression tuple

parseList :: Parser PhallExpression
parseList = do
  Lexer.tokenizeSymbol LeftSquareBracket
  list <- Megaparsec.many $ parseOptionalComma parseInnerExpression parseExpression
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
    parseBoolean =
      Megaparsec.choice
        [ True <$ Lexer.tokenizeKeyword TrueKeyword,
          False <$ Lexer.tokenizeKeyword FalseKeyword
        ]

parseVariable :: Parser PhallExpression
parseVariable =
  VariableExpression <$> Lexer.tokenizeIdentifier
