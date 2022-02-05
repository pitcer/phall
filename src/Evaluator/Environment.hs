{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator.Environment where

import Control.Monad.Except (Except)
import qualified Control.Monad.Except as Except (throwError)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Text.Lazy (Text)
import Error (EvaluatorError (..))
import qualified Evaluator.Builtin as Builtin (add, fold)
import Evaluator.PhallValue (ClosureInner (..), PhallValue (..))
import PhallParser (VariableName)

newtype Environment = Environment
  { variables :: Map Text PhallValue
  }
  deriving (Show)

empty :: Environment
empty = Environment {variables = Map.empty}

getVariable :: Environment -> VariableName -> Except EvaluatorError PhallValue
getVariable _ "add" =
  return . ClosureValue . ClosureInner $ \first ->
    return . ClosureValue . ClosureInner $ \second ->
      Builtin.add first second
getVariable _ "fold" =
  return . ClosureValue . ClosureInner $ \closure ->
    return . ClosureValue . ClosureInner $ \firstElement ->
      return . ClosureValue . ClosureInner $ \accumulator ->
        Builtin.fold closure firstElement accumulator
getVariable environment variableName =
  case Map.lookup variableName $ variables environment of
    Nothing -> Except.throwError VariableNotFound {variableName}
    Just variable -> return variable

withVariable :: Environment -> VariableName -> PhallValue -> Environment
withVariable environment variableName value =
  Environment {variables = Map.insert variableName value $ variables environment}
