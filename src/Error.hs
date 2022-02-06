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
  deriving (Show)

instance Error EvaluatorError where
  message InvalidTypeError {correctType, actualType} =
    "InvalidTypeError: Invalid type, expected '" <> correctType <> "' but get '" <> actualType
      <> "' instead"
  message VariableNotFound {variableName} =
    "VariableNotFound: Variable '" <> variableName <> "' not found in environment"

data TypeError
  = TypeMismatchError
      { expectedType :: Text,
        foundType :: Text,
        context :: Text
      }
  | TypeNotFoundError
      { typeVariableName :: Text
      }
  deriving (Show)

instance Error TypeError where
  message TypeMismatchError {expectedType, foundType, context} =
    "TypeMismatchError: Invalid type, expected '" <> expectedType <> "' but get '" <> foundType
      <> "' instead: "
      <> context
      <> "."
  message TypeNotFoundError {typeVariableName} =
    "TypeNotFoundError: Type for variable '" <> typeVariableName
      <> "' not found in type environment"
