{-# LANGUAGE NamedFieldPuns #-}

module Evaluator.ValueEnvironment where

import Common
import Control.Monad.Except as Except
import Environment
import Error
import Evaluator.PhallValue

type ValueEnvironment = Environment PhallValue

getVariable :: ValueEnvironment -> Name -> Result PhallValue
getVariable environment variableName =
  handleLookup $ Environment.lookup variableName environment
  where
    handleLookup :: Maybe PhallValue -> Result PhallValue
    handleLookup Nothing =
      Except.throwError VariableNotFound {variableName}
    handleLookup (Just variable) =
      return variable
