module Parser.PhallExpression where

import Data.Text.Lazy as Text
import Parser.PhallType

data PhallExpression
  = LambdaExpression
      { parameter :: VariableName,
        maybeParameterType :: Maybe PhallType,
        body :: PhallExpression,
        maybeBodyType :: Maybe PhallType
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
  | ListExpression [PhallExpression]
  | ConstantExpression PhallConstant
  | VariableExpression VariableName
  deriving (Show, Eq)

type VariableName = Text

data PhallConstant
  = BooleanConstant Bool
  | IntegerConstant Integer
  | FloatConstant Double
  | CharConstant Char
  | StringConstant Text
  deriving (Show, Eq)
