{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Error where

import Common
import Control.Monad.Except as Except
import Data.Text.Lazy as Text
import Text.Megaparsec as Megaparsec

type Result a = Except PhallError a

type ResultIO a = ExceptT PhallError IO a

toResultIO :: Result a -> ResultIO a
toResultIO = Except.liftEither . Except.runExcept

type ParserError = ParseErrorBundle ParsecStream ParsecError

data PhallError
  = ParserError ParserError
  | TypeMismatchError
      { expectedType :: Text,
        actualType :: Text,
        context :: Text
      }
  | TypeNotFoundError
      { typeVariableName :: Text
      }
  | VariableNotFound
      { variableName :: Text
      }
  | UnknownInternalCall
      { callName :: Text
      }
  | FieldNamesMismatchError
      { typeFieldName :: Text,
        actualFieldName :: Text
      }
  | UnexpectedExportError
  | MissingExportError
  | CustomError Text
  deriving (Show)

message :: PhallError -> Text
message (ParserError bundle) =
  Text.pack $ Megaparsec.errorBundlePretty bundle
message TypeMismatchError {expectedType, actualType, context} =
  "TypeMismatchError: Invalid type, expected '" <> expectedType <> "' but get '" <> actualType
    <> "' instead: "
    <> context
    <> "."
message TypeNotFoundError {typeVariableName} =
  "TypeNotFoundError: Type for variable '" <> typeVariableName
    <> "' not found in type environment"
message VariableNotFound {variableName} =
  "VariableNotFound: Variable '" <> variableName <> "' not found in environment"
message UnknownInternalCall {callName} =
  "UnknownInternalCall: @" <> callName <> "."
message UnexpectedExportError =
  "UnexpectedExportError: export occured in not imported expression."
message MissingExportError =
  "MissingExportError: export not occured in imported expression."
message (CustomError text) =
  "CustomError: " <> text
message FieldNamesMismatchError {typeFieldName, actualFieldName} =
  "FieldNamesMismatchError: Invalid field name, declared '" <> typeFieldName <> "' but get '"
    <> actualFieldName
    <> "'."
