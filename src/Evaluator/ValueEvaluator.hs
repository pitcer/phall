{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator.ValueEvaluator where

import Control.Monad as Monad
import qualified Control.Monad.Except as Except
import Data.Text.Lazy as Text
import Environment
import Error
import Evaluator.PhallValue as Value
import Evaluator.ValueEnvironment as ValueEnvironment
import Internal.Internal as Internal
import Parser.PhallExpression as Expression
import Parser.PhallType as Type

evaluate :: PhallExpression -> Result PhallValue
evaluate = evaluateValue Environment.empty

evaluateValue :: ValueEnvironment -> PhallExpression -> Result PhallValue
evaluateValue environment ImportExpression {importSource, importedItems, importBody} = do
  evaluated <- evaluateValue Environment.empty importSource
  evaluateExportBundle evaluated
  where
    evaluateExportBundle (ExportBundleValue exportedEnvironment) = do
      let restrictedEnvironment = Environment.restrict exportedEnvironment importedItems
      let extendedEnvironment = Environment.union environment restrictedEnvironment
      evaluateValue extendedEnvironment importBody
    evaluateExportBundle _ =
      Except.throwError MissingExportError
evaluateValue environment (ExportExpression exportedItems) = do
  let restrictedEnvironment = Environment.restrict environment exportedItems
  return $ ExportBundleValue restrictedEnvironment
evaluateValue environment TypeDeclarationExpression {typeDeclarationBody} =
  evaluateValue environment typeDeclarationBody
evaluateValue environment DataDeclarationExpression {declarationBody} =
  evaluateValue environment declarationBody
evaluateValue environment DataInstanceExpression {instanceFields} = do
  fields <- Monad.mapM evaluateField instanceFields
  return $ DataValue fields
  where
    evaluateField DataInstanceField {Expression.fieldName, fieldValue} = do
      value <- evaluateValue environment fieldValue
      return (fieldName, value)
evaluateValue environment InternalCallExpression {calleeName, arguments} = do
  evaluatedArguments <- Monad.mapM (evaluateValue environment) arguments
  Internal.internalCall calleeName evaluatedArguments
evaluateValue environment LambdaExpression {parameter, body} = do
  return $ ClosureValue . ClosureInner $ evaluateArgument
  where
    evaluateArgument argument = do
      let parameterName = Expression.parameterName parameter
      let extendedEnvironment = Environment.with parameterName argument environment
      evaluateValue extendedEnvironment body
evaluateValue environment ApplicationExpression {function, argument} = do
  evaluatedFunction <- evaluateValue environment function
  evaluateClosure evaluatedFunction
  where
    evaluateClosure (ClosureValue (ClosureInner closure)) = do
      evaluatedArgument <- evaluateValue environment argument
      closure evaluatedArgument
    evaluateClosure actualType =
      Except.throwError $
        TypeMismatchError
          { expectedType = "Closure",
            actualType = Type.getTypeName $ Value.getValueType actualType,
            context = ""
          }
evaluateValue environment (TupleExpression tuple) = do
  evaluatedTuple <- Monad.mapM (evaluateValue environment) tuple
  return $ TupleValue evaluatedTuple
evaluateValue environment (ListExpression list) = do
  evaluatedList <- Monad.mapM (evaluateValue environment) list
  return $ ListValue evaluatedList
evaluateValue environment ConditionalExpression {condition, positive, negative} = do
  conditionValue <- evaluateValue environment condition
  evaluateCondition conditionValue
  where
    evaluateCondition (BooleanValue value) =
      if value
        then evaluateValue environment positive
        else evaluateValue environment negative
    evaluateCondition actualType = do
      Except.throwError $
        TypeMismatchError
          { expectedType = "Bool",
            actualType = Type.getTypeName $ Value.getValueType actualType,
            context = ""
          }
evaluateValue _ (ConstantExpression constant) =
  return $ evaluateConstant constant
evaluateValue environment (VariableExpression name) =
  ValueEnvironment.getVariable environment name

evaluateConstant :: PhallConstant -> PhallValue
evaluateConstant UnitConstant = UnitValue
evaluateConstant (BooleanConstant boolean) = BooleanValue boolean
evaluateConstant (IntegerConstant integer) = IntegerValue integer
evaluateConstant (FloatConstant float) = FloatValue float
evaluateConstant (CharConstant char) = CharValue char
evaluateConstant (StringConstant string) = StringValue $ Text.unpack string
