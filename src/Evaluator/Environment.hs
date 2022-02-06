{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator.Environment where

import Control.Monad.Except as Except
import Data.Map as Map
import Data.Text.Lazy (Text)
import Error (EvaluatorError (..))
import Evaluator.Builtin as Builtin
import Evaluator.PhallValue (ClosureInner (..), PhallValue (..))
import Parser.PhallParser (Name)

newtype Environment = Environment
  { variables :: Map Text PhallValue
  }
  deriving (Show)

empty :: Environment
empty = Environment {variables = Map.empty}

getVariable :: Environment -> Name -> Except EvaluatorError PhallValue
getVariable _ "add" =
  return . ClosureValue . ClosureInner $ \first ->
    return . ClosureValue . ClosureInner $ \second ->
      Builtin.add first second
getVariable _ "sub" =
  return . ClosureValue . ClosureInner $ \first ->
    return . ClosureValue . ClosureInner $ \second ->
      Builtin.subtract first second
getVariable _ "mul" =
  return . ClosureValue . ClosureInner $ \first ->
    return . ClosureValue . ClosureInner $ \second ->
      Builtin.multiply first second
getVariable _ "div" =
  return . ClosureValue . ClosureInner $ \first ->
    return . ClosureValue . ClosureInner $ \second ->
      Builtin.divide first second
getVariable _ "fold" =
  return . ClosureValue . ClosureInner $ \closure ->
    return . ClosureValue . ClosureInner $ \firstElement ->
      return . ClosureValue . ClosureInner $ \accumulator ->
        Builtin.fold closure firstElement accumulator
getVariable _ "isEqual" =
  return . ClosureValue . ClosureInner $ \first ->
    return . ClosureValue . ClosureInner $ \second ->
      Builtin.isEqual first second
getVariable environment variableName =
  case Map.lookup variableName $ variables environment of
    Nothing -> Except.throwError VariableNotFound {variableName}
    Just variable -> return variable

withVariable :: Environment -> Name -> PhallValue -> Environment
withVariable environment variableName value =
  Environment {variables = Map.insert variableName value $ variables environment}
