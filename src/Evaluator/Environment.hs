{-# LANGUAGE NamedFieldPuns #-}

module Evaluator.Environment where

import Control.Monad.Except (Except)
import qualified Control.Monad.Except as Except (throwError)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Text.Lazy (Text)
import Error (EvaluatorError (..))
import PhallParser (VariableName)

newtype Environment a = Environment
  { variables :: Map Text a
  }
  deriving (Show)

empty :: Environment a
empty = Environment {variables = Map.empty}

getVariable :: Environment a -> VariableName -> Except EvaluatorError a
getVariable environment variableName =
  case Map.lookup variableName $ variables environment of
    Nothing -> Except.throwError VariableNotFound {variableName}
    Just variable -> return variable

withVariable :: Environment a -> VariableName -> a -> Environment a
withVariable environment variableName value =
  Environment {variables = Map.insert variableName value $ variables environment}
