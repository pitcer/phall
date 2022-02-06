{-# LANGUAGE OverloadedStrings #-}

module TypeEvaluator.TypeEnvironment where

import Control.Monad.Except as Except
import Data.Map as Map
import Data.Text.Lazy (Text)
import Error (TypeError (..))
import Evaluator.Builtin as Builtin
import Parser.PhallType

newtype TypeEnvironment = TypeEnvironment
  { getVariables :: Map Text PhallType
  }
  deriving (Show)

empty :: TypeEnvironment
empty = TypeEnvironment {getVariables = Map.empty}

getType :: TypeEnvironment -> Name -> Except TypeError PhallType
getType _ "add" = return Builtin.arithmeticOperationType
getType _ "sub" = return Builtin.arithmeticOperationType
getType _ "mul" = return Builtin.arithmeticOperationType
getType _ "div" = return Builtin.arithmeticOperationType
getType _ "fold" = return Builtin.foldType
getType _ "isEqual" = return Builtin.isEqualType
getType environment variableName =
  handleLookup . Map.lookup variableName $ getVariables environment
  where
    handleLookup :: Maybe PhallType -> Except TypeError PhallType
    handleLookup Nothing =
      Except.throwError TypeNotFoundError {typeVariableName = variableName}
    handleLookup (Just variable) =
      return variable

withVariable :: TypeEnvironment -> Name -> PhallType -> TypeEnvironment
withVariable environment variableName value = do
  let variables = Map.insert variableName value $ getVariables environment
  environment {getVariables = variables}
