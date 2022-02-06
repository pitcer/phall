{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator.PhallEvaluator where

import Control.Monad as Monad
import Control.Monad.Except (Except)
import qualified Control.Monad.Except as Except
import Data.Map as Map
import Error (EvaluatorError (..))
import Evaluator.Environment (Environment)
import qualified Evaluator.Environment as Environment
import Evaluator.PhallValue
import Parser.PhallExpression as Expression

evaluate :: Environment -> PhallExpression -> Except EvaluatorError PhallValue
evaluate environment DataDeclarationExpression {declarationBody} =
  evaluate environment declarationBody
evaluate environment DataInstanceExpression {instanceFields} = do
  fields <- Monad.foldM evaluateField Map.empty instanceFields
  return $ DataValue fields
  where
    evaluateField fields DataInstanceField {fieldName, fieldValue} = do
      value <- evaluate environment fieldValue
      return $ Map.insert fieldName value fields
evaluate environment LambdaExpression {parameter, body} =
  return . ClosureValue . ClosureInner $ evaluateArgument
  where
    evaluateArgument argument =
      evaluate (Environment.withVariable environment parameter argument) body
evaluate environment ApplicationExpression {function, argument} = do
  evaluatedFunction <- evaluate environment function
  evaluateClosure evaluatedFunction
  where
    evaluateClosure (ClosureValue (ClosureInner closure)) = do
      evaluatedArgument <- evaluate environment argument
      closure evaluatedArgument
    evaluateClosure _ =
      Except.throwError $ InvalidTypeError {correctType = "Closure", actualType = "?"}
evaluate environment (ListExpression list) = do
  evaluatedList <- mapM (evaluate environment) list
  return $ ListValue evaluatedList
evaluate environment ConditionalExpression {condition, positive, negative} = do
  value <- evaluate environment condition
  evaluateValue value
  where
    evaluateValue (BooleanValue value) =
      if value
        then evaluate environment positive
        else evaluate environment negative
    evaluateValue _ = do
      Except.throwError $ InvalidTypeError {correctType = "Boolean", actualType = "?"}
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
