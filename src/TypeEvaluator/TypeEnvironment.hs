module TypeEvaluator.TypeEnvironment where

import Common
import Control.Monad.Except as Except
import Environment
import Error
import Parser.PhallType

type TypeEnvironment = Environment PhallType

getType :: TypeEnvironment -> Name -> Result PhallType
getType environment typeName =
  handleLookup $ Environment.lookup typeName environment
  where
    handleLookup :: Maybe PhallType -> Result PhallType
    handleLookup Nothing =
      Except.throwError TypeNotFoundError {typeVariableName = typeName}
    handleLookup (Just variable) =
      return variable
