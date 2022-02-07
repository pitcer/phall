{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Error
  ( PhallError (..),
    EvaluatorError (..),
    TypeError (..),
    ParserError,
    Error (..),
  )
where

import Common
import Data.Text.Lazy as Text
import Text.Megaparsec as Megaparsec

class Error a where
  message :: a -> Text

data PhallError
  = ParserError ParserError
  | TypeError TypeError
  | EvaluatorError EvaluatorError
  deriving (Show)

type ParserError = ParseErrorBundle ParsecStream ParsecError

data EvaluatorError
  = InvalidTypeError
      { correctType :: Text,
        actualType :: Text
      }
  | VariableNotFound
      { variableName :: Text
      }
  | MissingExportInImportedExpressionEvaluatorError
  | CustomError Text
  deriving (Show)

instance Error EvaluatorError where
  message InvalidTypeError {correctType, actualType} =
    "InvalidTypeError: Invalid type, expected '" <> correctType <> "' but get '" <> actualType
      <> "' instead"
  message VariableNotFound {variableName} =
    "VariableNotFound: Variable '" <> variableName <> "' not found in environment"
  message MissingExportInImportedExpressionEvaluatorError =
    "MissingExportInImportedExpressionEvaluatorError: export not occured in imported expression"
  message (CustomError text) =
    "CustomError: " <> text

data TypeError
  = TypeMismatchError
      { expectedType :: Text,
        foundType :: Text,
        context :: Text
      }
  | FieldNamesMismatchError
      { typeFieldName :: Text,
        actualFieldName :: Text
      }
  | TypeNotFoundError
      { typeVariableName :: Text
      }
  | ExportInOuterExpressionTypeError
  | MissingExportInImportedExpressionTypeError
  deriving (Show)

instance Error TypeError where
  message TypeMismatchError {expectedType, foundType, context} =
    "TypeMismatchError: Invalid type, expected '" <> expectedType <> "' but get '" <> foundType
      <> "' instead: "
      <> context
      <> "."
  message FieldNamesMismatchError {typeFieldName, actualFieldName} =
    "FieldNamesMismatchError: Invalid field name, declared '" <> typeFieldName <> "' but get '"
      <> actualFieldName
      <> "'."
  message TypeNotFoundError {typeVariableName} =
    "TypeNotFoundError: Type for variable '" <> typeVariableName
      <> "' not found in type environment"
  message ExportInOuterExpressionTypeError =
    "ExportInOuterExpressionTypeError: export occured in the most outer expression."
  message MissingExportInImportedExpressionTypeError =
    "MissingExportInInnerExpressionTypeError: export not occured in imported expression"
