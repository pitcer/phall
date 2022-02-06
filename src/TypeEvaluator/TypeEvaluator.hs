{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeEvaluator.TypeEvaluator where

import Control.Monad.Except as Except
import Error (TypeError (..))
import Parser.PhallExpression
import Parser.PhallType (PhallConstantType (..), PhallType (..))
import qualified Parser.PhallType as Type
import TypeEvaluator.TypeEnvironment as TypeEnvironment

evaluate :: PhallExpression -> Except TypeError (PhallExpression, PhallType)
evaluate = evaluateType TypeEnvironment.empty

evaluateType :: TypeEnvironment -> PhallExpression -> Except TypeError (PhallExpression, PhallType)
evaluateType environment LambdaExpression {parameter, maybeParameterType, body, maybeBodyType} = do
  let parameterType = evaluateMaybeType maybeParameterType
  let bodyEnvironment = TypeEnvironment.withVariable environment parameter parameterType
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
    createLambdaResult parameterType evaluatedBody evaluatedBodyType =
      ( LambdaExpression
          { parameter,
            maybeParameterType,
            body = evaluatedBody,
            maybeBodyType = Just evaluatedBodyType
          },
        LambdaType
          { Type.parameterType = parameterType,
            Type.bodyType = evaluatedBodyType
          }
      )
    evaluateMaybeType Nothing = AnyType -- TODO: get from body context
    evaluateMaybeType (Just justType) = justType
evaluateType environment ApplicationExpression {function, argument} = do
  (functionExpression, functionType) <- evaluateType environment function
  case functionType of
    LambdaType {Type.parameterType, Type.bodyType} -> do
      (argumentExpression, argumentType) <- evaluateType environment argument
      if parameterType == argumentType
        then
          return
            ( ApplicationExpression
                { function = functionExpression,
                  argument = argumentExpression
                },
              bodyType
            )
        else
          Except.throwError $
            TypeMismatchError
              { expectedType = Type.getTypeName parameterType,
                foundType = Type.getTypeName argumentType,
                context = "evaluate application expression"
              }
    _ ->
      Except.throwError $
        TypeMismatchError
          { expectedType = "Closure",
            foundType = "?",
            context = "evaluate application expression"
          }
evaluateType environment (ListExpression list) = do
  evaluatedList <- mapM (evaluateType environment) list
  let (patchedList, elementsTypes) = unzip evaluatedList
  let listType = getListType elementsTypes
  if Prelude.all (listType ==) elementsTypes
    then return (ListExpression patchedList, ListType listType)
    else
      Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName listType,
            foundType = "",
            context = "evaluate list expression"
          }
  where
    getListType [] = AnyType
    getListType types = Prelude.head types
evaluateType environment ConditionalExpression {condition, positive, negative} = do
  (conditionExpression, conditionType) <- evaluateType environment condition
  case conditionType of
    ConstantType BooleanType -> do
      (positiveExpression, positiveType) <- evaluateType environment positive
      (negativeExpression, negativeType) <- evaluateType environment negative
      if positiveType == negativeType
        then
          return
            ( ConditionalExpression
                { condition = conditionExpression,
                  positive = positiveExpression,
                  negative = negativeExpression
                },
              positiveType
            )
        else
          Except.throwError $
            TypeMismatchError
              { expectedType = Type.getTypeName positiveType,
                foundType = Type.getTypeName negativeType,
                context = "evaluate conditional expression"
              }
    foundType ->
      Except.throwError $
        TypeMismatchError
          { expectedType = "Boolean",
            foundType = Type.getTypeName foundType,
            context = "evalueate conditional expression"
          }
evaluateType _ expression@(ConstantExpression constant) =
  return (expression, evaluateConstantType constant)
evaluateType environment expression@(VariableExpression name) = do
  variableType <- TypeEnvironment.getType environment name
  return (expression, variableType)

evaluateConstantType :: PhallConstant -> PhallType
evaluateConstantType (BooleanConstant _) = ConstantType BooleanType
evaluateConstantType (IntegerConstant _) = ConstantType IntegerType
evaluateConstantType (FloatConstant _) = ConstantType FloatType
evaluateConstantType (CharConstant _) = ConstantType CharType
evaluateConstantType (StringConstant _) = ConstantType StringType
