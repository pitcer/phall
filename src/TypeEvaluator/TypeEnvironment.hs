module TypeEvaluator.TypeEnvironment where

import Control.Monad.Except as Except
import Environment
import Error (TypeError (..))
import Parser.PhallType

type TypeEnvironment = Environment PhallType

getType :: TypeEnvironment -> Name -> Except TypeError PhallType
getType environment typeName =
  handleLookup $ Environment.lookup typeName environment
  where
    handleLookup :: Maybe PhallType -> Except TypeError PhallType
    handleLookup Nothing =
      Except.throwError TypeNotFoundError {typeVariableName = typeName}
    handleLookup (Just variable) =
      return variable
