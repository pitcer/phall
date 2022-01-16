{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Error
  ( PhallError (..),
    EvaluatorError (..),
    ParserError,
    Error (..),
  )
where

import Common
import Data.Text.Lazy (Text)
import Text.Megaparsec (ParseErrorBundle)

class Error a where
  message :: a -> Text

data PhallError
  = EvaluatorError EvaluatorError
  | ParserError ParserError
  deriving (Show)

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
    "Invalid type, expected '" <> correctType <> "' but get '" <> actualType <> "' instead"
  message VariableNotFound {variableName} =
    "Variable '" <> variableName <> "' not found in environment"

type ParserError = ParseErrorBundle ParsecStream ParsecError
