{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator.PhallEvaluator where

import Control.Monad.Except (Except)
import qualified Control.Monad.Except as Except (throwError)
import Data.Text.Lazy (Text)
import Error (EvaluatorError (..))
import qualified Evaluator.Environment as Environment (Environment, getVariable, withVariable)
import PhallParser

type Environment = Environment.Environment PhallValue

data PhallValue
  = BooleanValue Bool
  | IntegerValue Integer
  | FloatValue Double
  | CharValue Char
  | StringValue Text
  | ClosureValue
      { parameter :: VariableName,
        body :: PhallExpression,
        environment :: Environment
      }
  deriving (Show)

evaluate :: Environment -> PhallExpression -> Except EvaluatorError PhallValue
evaluate environment LambdaExpression {parameter, body} =
  return $ ClosureValue {parameter, body, environment}
evaluate environment ApplicationExpression {function, argument} = do
  functionValue <- evaluate environment function
  case functionValue of
    ClosureValue {parameter, body, environment = closureEnvironment} -> do
      evaluatedArgument <- evaluate environment argument
      evaluate (Environment.withVariable closureEnvironment parameter evaluatedArgument) body
    _ -> Except.throwError $ InvalidTypeError {correctType = "Closure", actualType = "?"}
evaluate environment ConditionalExpression {condition, positive, negative} = do
  value <- evaluate environment condition
  case value of
    BooleanValue booleanValue ->
      if booleanValue
        then evaluate environment positive
        else evaluate environment negative
    _ -> Except.throwError $ InvalidTypeError {correctType = "Boolean", actualType = "?"}
evaluate _ (ConstantExpression constant) =
  return $ evaluateConstant constant
evaluate environment (VariableExpression name) =
  Environment.getVariable environment name

evaluateConstant :: PhallConstant -> PhallValue
evaluateConstant (BooleanConstant boolean) = BooleanValue boolean
evaluateConstant (IntegerConstant integer) = IntegerValue integer
evaluateConstant (FloatConstant float) = FloatValue float
evaluateConstant (CharConstant char) = CharValue char
evaluateConstant (StringConstant string) = StringValue string
