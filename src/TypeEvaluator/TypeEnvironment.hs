{-# LANGUAGE OverloadedStrings #-}

module TypeEvaluator.TypeEnvironment where

import Control.Monad.Except as Except
import Environment
import Error (TypeError (..))
import Evaluator.Builtin as Builtin
import Parser.PhallType

type TypeEnvironment = Environment PhallType

getType :: TypeEnvironment -> Name -> Except TypeError PhallType
getType _ "add" = return Builtin.arithmeticOperationType
getType _ "sub" = return Builtin.arithmeticOperationType
getType _ "mul" = return Builtin.arithmeticOperationType
getType _ "div" = return Builtin.arithmeticOperationType
getType _ "fold" = return Builtin.foldType
getType _ "isEqual" = return Builtin.isEqualType
getType environment typeName =
  handleLookup $ Environment.lookup typeName environment
  where
    handleLookup :: Maybe PhallType -> Except TypeError PhallType
    handleLookup Nothing =
      Except.throwError TypeNotFoundError {typeVariableName = typeName}
    handleLookup (Just variable) =
      return variable
