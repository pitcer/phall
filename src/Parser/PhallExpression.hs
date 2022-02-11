module Parser.PhallExpression where

import Data.Text.Lazy as Text
import FullSet
import Parser.PhallType

data PhallExpression
  = ImportExpression
      { importSource :: PhallExpression,
        importedItems :: FullSet Name,
        importBody :: PhallExpression
      }
  | ExportExpression (FullSet Name)
  | DataDeclarationExpression
      { declarationName :: Name,
        declarationFields :: [DataTypeField],
        declarationBody :: PhallExpression
      }
  | DataInstanceExpression
      { instanceName :: Name,
        instanceFields :: [DataInstanceField]
      }
  | InternalCallExpression
      { calleeName :: Name,
        arguments :: [PhallExpression]
      }
  | LambdaExpression
      { parameter :: LambdaParameter,
        body :: PhallExpression,
        bodyType :: PhallType
      }
  | ApplicationExpression
      { function :: PhallExpression,
        argument :: PhallExpression
      }
  | ConditionalExpression
      { condition :: PhallExpression,
        positive :: PhallExpression,
        negative :: PhallExpression
      }
  | TupleExpression [PhallExpression]
  | ListExpression [PhallExpression]
  | ConstantExpression PhallConstant
  | VariableExpression Name
  deriving (Show, Eq)

data LambdaParameter = LambdaParameter
  { parameterName :: Name,
    parameterType :: PhallType
  }
  deriving (Show, Eq)

data DataInstanceField = DataInstanceField
  { fieldName :: Name,
    fieldValue :: PhallExpression
  }
  deriving (Show, Eq)

data PhallConstant
  = BooleanConstant Bool
  | IntegerConstant Integer
  | FloatConstant Double
  | CharConstant Char
  | StringConstant Text
  deriving (Show, Eq)
