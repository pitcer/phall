{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator.ValueEnvironment where

import Control.Monad.Except as Except
import Environment
import Error (EvaluatorError (..))
import Evaluator.Builtin as Builtin
import Evaluator.PhallValue (ClosureInner (..), PhallValue (..))
import Parser.PhallType

type ValueEnvironment = Environment PhallValue

getVariable :: ValueEnvironment -> Name -> Except EvaluatorError PhallValue
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
  handleLookup $ Environment.lookup variableName environment
  where
    handleLookup :: Maybe PhallValue -> Except EvaluatorError PhallValue
    handleLookup Nothing =
      Except.throwError VariableNotFound {variableName}
    handleLookup (Just variable) =
      return variable
