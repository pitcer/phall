{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeEvaluator.TypeEvaluator where

import Control.Monad as Monad
import Control.Monad.Except as Except
import Control.Monad.State as State
import qualified Data.List as List
import Data.Maybe as Maybe
import Data.Text.Lazy as Text
import Environment
import Error (TypeError (..))
import ListT
import Parser.PhallExpression as Expression
import Parser.PhallType as Type
import TypeEvaluator.TypeEnvironment as TypeEnvironment

type TypeEvaluatorResult = Except TypeError

evaluate :: PhallExpression -> TypeEvaluatorResult (PhallExpression, PhallType)
evaluate expression = do
  let evaluated = ListT.head $ evaluateType Environment.empty expression
  maybeResult <- State.evalStateT evaluated Environment.empty
  maybe (Except.throwError ExportInOuterExpressionTypeError) return maybeResult

type TypeEvaluatorMonad = ListT (StateT TypeEvaluatorState TypeEvaluatorResult)

type TypeEvaluatorState = TypeEnvironment

liftExcept :: Except TypeError a -> TypeEvaluatorMonad a
liftExcept = lift . lift

evaluateType ::
  TypeEnvironment ->
  PhallExpression ->
  TypeEvaluatorMonad (PhallExpression, PhallType)
evaluateType environment ImportExpression {importSource, importedItems, importBody} = do
  let evaluated = ListT.head $ evaluateType Environment.empty importSource
  (result, exportedEnvironment) <- liftExcept $ State.runStateT evaluated Environment.empty
  Monad.unless (Maybe.isNothing result) . Except.throwError $
    MissingExportInImportedExpressionTypeError
  let restrictedEnvironment = Environment.restrict exportedEnvironment importedItems
  let extendedEnvironment = Environment.union environment restrictedEnvironment
  evaluateType extendedEnvironment importBody
evaluateType environment (ExportExpression exportedItems) = do
  let restrictedEnvironment = Environment.restrict environment exportedItems
  State.modify $ Environment.union restrictedEnvironment
  mzero
evaluateType
  environment
  DataDeclarationExpression {declarationName, declarationFields, declarationBody} = do
    let declarationType = DataType declarationFields
    let bodyEnvironment = Environment.with declarationName declarationType environment
    (filledBody, bodyType) <- evaluateType bodyEnvironment declarationBody
    let expression =
          DataDeclarationExpression
            { declarationName,
              declarationFields,
              declarationBody = filledBody
            }
    return (expression, bodyType)
evaluateType environment DataInstanceExpression {instanceName, instanceFields} = do
  instanceType <- liftExcept $ TypeEnvironment.getType environment instanceName
  evaluateDataType instanceType
  where
    evaluateDataType (DataType typeFields) = do
      let sortedTypeFields = List.sortOn Type.fieldName typeFields
      let sortedInstanceFields = List.sortOn Expression.fieldName instanceFields
      evaluatedInstanceFields <- Monad.mapM evaluateInstanceField sortedInstanceFields
      validatedFields <-
        liftExcept $ Monad.zipWithM validateField sortedTypeFields evaluatedInstanceFields
      let result = DataInstanceExpression {instanceName, instanceFields = validatedFields}
      let resultType = NamedType instanceName
      return (result, resultType)
    evaluateDataType instanceType =
      Except.throwError $
        TypeMismatchError
          { expectedType = "DataType(" <> instanceName <> ")",
            foundType = Type.getTypeName instanceType,
            context = "evaluate data instance expression"
          }

    evaluateInstanceField DataInstanceField {Expression.fieldName, fieldValue} = do
      (evaluatedValue, evaluatedType) <- evaluateType environment fieldValue
      let result = DataInstanceField {Expression.fieldName, fieldValue = evaluatedValue}
      return (result, evaluatedType)

    validateField ::
      DataTypeField -> (DataInstanceField, PhallType) -> Except TypeError DataInstanceField
    validateField
      DataTypeField {Type.fieldName = typeFieldName, Type.fieldType}
      (field@DataInstanceField {Expression.fieldName}, evaluatedType) = do
        Monad.unless (fieldName == typeFieldName) . Except.throwError $
          FieldNamesMismatchError {typeFieldName, actualFieldName = fieldName}
        Monad.unless (evaluatedType == fieldType) . Except.throwError $
          TypeMismatchError
            { expectedType = Type.getTypeName fieldType,
              foundType = Type.getTypeName evaluatedType,
              context = "evaluate data instance expression"
            }
        return field
evaluateType environment LambdaExpression {parameter, body, maybeBodyType} = do
  let parameterType = evaluateMaybeType $ Expression.maybeParameterType parameter
  let parameterName = Expression.parameterName parameter
  let bodyEnvironment = Environment.with parameterName parameterType environment
  (evaluatedBody, evaluatedBodyType) <- evaluateType bodyEnvironment body
  case maybeBodyType of
    Nothing ->
      return $ createLambdaResult parameterType evaluatedBody evaluatedBodyType
    Just bodyType
      | bodyType == evaluatedBodyType ->
        return $ createLambdaResult parameterType evaluatedBody evaluatedBodyType
    Just bodyType ->
      Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName bodyType,
            foundType = Type.getTypeName evaluatedBodyType,
            context = "evaluate lambda expression"
          }
  where
    createLambdaResult parameterType evaluatedBody evaluatedBodyType = do
      let result =
            LambdaExpression
              { parameter,
                body = evaluatedBody,
                maybeBodyType = Just evaluatedBodyType
              }
      let resultType =
            LambdaType
              { Type.parameterType = parameterType,
                Type.bodyType = evaluatedBodyType
              }
      (result, resultType)

    evaluateMaybeType Nothing = AnyType -- TODO: get from body context
    evaluateMaybeType (Just justType) = justType
evaluateType environment ApplicationExpression {function, argument} = do
  (functionExpression, functionType) <- evaluateType environment function
  evaluateFunctionType functionExpression functionType
  where
    evaluateFunctionType functionExpression LambdaType {Type.parameterType, Type.bodyType} = do
      (argumentExpression, argumentType) <- evaluateType environment argument
      Monad.unless (parameterType == argumentType) . Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName parameterType,
            foundType = Type.getTypeName argumentType,
            context = "evaluate application expression"
          }
      let result =
            ApplicationExpression
              { function = functionExpression,
                argument = argumentExpression
              }
      return (result, bodyType)
    -- TODO: remove temporary fix
    evaluateFunctionType functionExpression AnyType = do
      (argumentExpression, _) <- evaluateType environment argument
      let result =
            ApplicationExpression
              { function = functionExpression,
                argument = argumentExpression
              }
      return (result, AnyType)
    --
    evaluateFunctionType _ functionType =
      Except.throwError $
        TypeMismatchError
          { expectedType = "Lambda",
            foundType = Type.getTypeName functionType,
            context = "evaluate application expression"
          }
evaluateType environment (ListExpression list) = do
  evaluatedList <- mapM (evaluateType environment) list
  let (patchedList, elementsTypes) = unzip evaluatedList
  let listType = getListType elementsTypes
  Monad.unless (Prelude.all (listType ==) elementsTypes) . Except.throwError $
    TypeMismatchError
      { expectedType = Type.getTypeName listType,
        foundType =
          "[" <> (Text.intercalate "," . Prelude.map Type.getTypeName $ elementsTypes) <> "]",
        context = "evaluate list expression"
      }
  return (ListExpression patchedList, ListType listType)
  where
    getListType [] = AnyType
    getListType types = Prelude.head types
evaluateType environment ConditionalExpression {condition, positive, negative} = do
  (conditionExpression, conditionType) <- evaluateType environment condition
  evaluateConditionType conditionExpression conditionType
  where
    evaluateConditionType conditionExpression (ConstantType BooleanType) = do
      (positiveExpression, positiveType) <- evaluateType environment positive
      (negativeExpression, negativeType) <- evaluateType environment negative
      Monad.unless (positiveType == negativeType) . Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName positiveType,
            foundType = Type.getTypeName negativeType,
            context = "evaluate conditional expression"
          }
      let result =
            ConditionalExpression
              { condition = conditionExpression,
                positive = positiveExpression,
                negative = negativeExpression
              }
      return (result, positiveType)
    evaluateConditionType _ conditionType =
      Except.throwError $
        TypeMismatchError
          { expectedType = "Boolean",
            foundType = Type.getTypeName conditionType,
            context = "evalueate conditional expression"
          }
evaluateType _ expression@(ConstantExpression constant) =
  return (expression, evaluateConstantType constant)
evaluateType environment expression@(VariableExpression name) = do
  variableType <- liftExcept $ TypeEnvironment.getType environment name
  return (expression, variableType)

evaluateConstantType :: PhallConstant -> PhallType
evaluateConstantType (BooleanConstant _) = ConstantType BooleanType
evaluateConstantType (IntegerConstant _) = ConstantType IntegerType
evaluateConstantType (FloatConstant _) = ConstantType FloatType
evaluateConstantType (CharConstant _) = ConstantType CharType
evaluateConstantType (StringConstant _) = ConstantType StringType
