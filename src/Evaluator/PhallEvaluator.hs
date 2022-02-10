{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluator.PhallEvaluator where

import Control.Monad as Monad
import Control.Monad.Except (Except)
import qualified Control.Monad.Except as Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.State as State
import Data.Map as Map
import Data.Maybe as Maybe
import Environment
import Error (EvaluatorError (..))
import Evaluator.PhallValue
import Evaluator.ValueEnvironment as ValueEnvironment
import ListT
import Parser.PhallExpression as Expression

type ValueEvaluatorResult = Except EvaluatorError

evaluate :: PhallExpression -> ValueEvaluatorResult PhallValue
evaluate expression = do
  let evaluated = ListT.head $ evaluateValue Environment.empty expression
  maybeResult <- State.evalStateT evaluated Environment.empty
  maybe (Except.throwError $ CustomError "evaluate with export") return maybeResult

type ValueEvaluatorMonad = ListT (StateT ValueEvaluatorState ValueEvaluatorResult)

type ValueEvaluatorState = ValueEnvironment

liftExcept :: ValueEvaluatorResult a -> ValueEvaluatorMonad a
liftExcept = lift . lift

evaluateValue :: ValueEnvironment -> PhallExpression -> ValueEvaluatorMonad PhallValue
evaluateValue environment ImportExpression {importSource, importedItems, importBody} = do
  let evaluated = ListT.head $ evaluateValue Environment.empty importSource
  (result, exportedEnvironment) <- liftExcept $ State.runStateT evaluated Environment.empty
  Monad.unless (Maybe.isNothing result) . Except.throwError $
    MissingExportInImportedExpressionEvaluatorError
  let restrictedEnvironment = Environment.restrict exportedEnvironment importedItems
  let extendedEnvironment = Environment.union environment restrictedEnvironment
  evaluateValue extendedEnvironment importBody
evaluateValue environment (ExportExpression exportedItems) = do
  let restrictedEnvironment = Environment.restrict environment exportedItems
  lift $ State.modify $ Environment.union restrictedEnvironment
  mzero
evaluateValue environment DataDeclarationExpression {declarationBody} =
  evaluateValue environment declarationBody
evaluateValue environment DataInstanceExpression {instanceFields} = do
  fields <- Monad.foldM evaluateField Map.empty instanceFields
  return $ DataValue fields
  where
    evaluateField fields DataInstanceField {fieldName, fieldValue} = do
      value <- evaluateValue environment fieldValue
      return $ Map.insert fieldName value fields
evaluateValue environment LambdaExpression {parameter, body} =
  return $ ClosureValue . ClosureInner $ evaluateArgument
  where
    evaluateArgument argument = do
      -- TODO: remove this temporary fix
      let parameterName = Expression.parameterName parameter
      let extendedEnvironment = Environment.with parameterName argument environment
      let evaluated = ListT.head $ evaluateValue extendedEnvironment body
      maybeResult <- State.evalStateT evaluated Environment.empty
      maybe (Except.throwError $ CustomError "lambda with export") return maybeResult
evaluateValue environment ApplicationExpression {function, argument} = do
  evaluatedFunction <- evaluateValue environment function
  evaluateClosure evaluatedFunction
  where
    evaluateClosure (ClosureValue (ClosureInner closure)) = do
      evaluatedArgument <- evaluateValue environment argument
      liftExcept $ closure evaluatedArgument
    evaluateClosure _ =
      Except.throwError $ InvalidTypeError {correctType = "Closure", actualType = "?"}
evaluateValue environment (ListExpression list) = do
  evaluatedList <- mapM (evaluateValue environment) list
  return $ ListValue evaluatedList
evaluateValue environment ConditionalExpression {condition, positive, negative} = do
  conditionValue <- evaluateValue environment condition
  evaluateCondition conditionValue
  where
    evaluateCondition (BooleanValue value) =
      if value
        then evaluateValue environment positive
        else evaluateValue environment negative
    evaluateCondition _ = do
      Except.throwError $ InvalidTypeError {correctType = "Boolean", actualType = "?"}
evaluateValue _ (ConstantExpression constant) =
  return $ evaluateConstant constant
evaluateValue environment (VariableExpression name) =
  liftExcept $ ValueEnvironment.getVariable environment name

evaluateConstant :: PhallConstant -> PhallValue
evaluateConstant (BooleanConstant boolean) = BooleanValue boolean
evaluateConstant (IntegerConstant integer) = IntegerValue integer
evaluateConstant (FloatConstant float) = FloatValue float
evaluateConstant (CharConstant char) = CharValue char
evaluateConstant (StringConstant string) = StringValue string
