{-# LANGUAGE OverloadedStrings #-}

module TypeEvaluator.TypeEnvironment where

import Control.Monad.Except as Except
import Data.Map as Map
import Data.Text.Lazy (Text)
import Error (TypeError (..))
import Evaluator.Builtin as Builtin
import Parser.PhallParser (VariableName)
import Parser.PhallType

newtype TypeEnvironment = TypeEnvironment
  { variables :: Map Text PhallType
  }
  deriving (Show)

empty :: TypeEnvironment
empty = TypeEnvironment {variables = Map.empty}

getType :: TypeEnvironment -> VariableName -> Except TypeError PhallType
getType _ "add" = return Builtin.arithmeticOperationType
getType _ "sub" = return Builtin.arithmeticOperationType
getType _ "mul" = return Builtin.arithmeticOperationType
getType _ "div" = return Builtin.arithmeticOperationType
getType _ "fold" = return Builtin.foldType
getType _ "isEqual" = return Builtin.isEqualType
getType environment variableName =
  case Map.lookup variableName $ variables environment of
    Nothing -> Except.throwError TypeNotFoundError {typeVariableName = variableName}
    Just variable -> return variable

withVariable :: TypeEnvironment -> VariableName -> PhallType -> TypeEnvironment
withVariable environment variableName value =
  TypeEnvironment {variables = Map.insert variableName value $ variables environment}
