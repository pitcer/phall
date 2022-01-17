{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
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
  | ListValue [PhallValue]
  | ClosureValue ClosureInner
  deriving (Show)

newtype ClosureInner
  = ClosureInner (PhallValue -> Except EvaluatorError PhallValue)

instance Show ClosureInner where
  show _ = "Closure"

evaluate :: Environment -> PhallExpression -> Except EvaluatorError PhallValue
evaluate environment LambdaExpression {parameter, body} =
  return . ClosureValue . ClosureInner $
    \argument -> evaluate (Environment.withVariable environment parameter argument) body
evaluate environment ApplicationExpression {function, argument} = do
  evaluatedFunction <- evaluate environment function
  case evaluatedFunction of
    ClosureValue (ClosureInner closure) -> do
      evaluatedArgument <- evaluate environment argument
      closure evaluatedArgument
    _ -> Except.throwError $ InvalidTypeError {correctType = "Closure", actualType = "?"}
evaluate environment (ListExpression list) = do
  evaluatedList <- mapM (evaluate environment) list
  return $ ListValue evaluatedList
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
