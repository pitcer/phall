{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeEvaluator.TypeEvaluator where

import Common
import Control.Monad as Monad
import Control.Monad.Except as Except
import qualified Data.List as List
import Data.Map as Map
import Data.Text.Lazy as Text
import Environment
import Error
import Internal.InternalType as Internal
import Parser.PhallExpression as Expression
import Parser.PhallType as Type
import TypeEvaluator.TypeEnvironment as TypeEnvironment

evaluate :: PhallExpression -> Result PhallType
evaluate = evaluateType Environment.empty

evaluateType :: TypeEnvironment -> PhallExpression -> Result PhallType
evaluateType environment ImportExpression {importSource, importedItems, importBody} = do
  sourceType <- evaluateType Environment.empty importSource
  evaluateExportBundle sourceType
  where
    evaluateExportBundle (ExportBundleType exportedEnvironment) = do
      let restrictedEnvironment = Environment.restrict exportedEnvironment importedItems
      let extendedEnvironment = Environment.union environment restrictedEnvironment
      evaluateType extendedEnvironment importBody
    evaluateExportBundle _ =
      Except.throwError MissingExportError
evaluateType environment (ExportExpression exportedItems) = do
  let restrictedEnvironment = Environment.restrict environment exportedItems
  return (ExportBundleType restrictedEnvironment)
evaluateType
  environment
  TypeDeclarationExpression {typeDeclarationName, typeDeclarationType, typeDeclarationBody} = do
    let bodyEnvironment = Environment.with typeDeclarationName typeDeclarationType environment
    evaluateType bodyEnvironment typeDeclarationBody
evaluateType
  environment
  DataDeclarationExpression {declarationName, declarationFields, declarationBody} = do
    Monad.mapM_ validateFieldDefaultValueType declarationFields
    let declarationType = DataType $ List.map Expression.toTypeField declarationFields
    let bodyEnvironment = Environment.with declarationName declarationType environment
    evaluateType bodyEnvironment declarationBody
    where
      validateFieldDefaultValueType DataDeclarationField {declarationFieldDefaultValue = Nothing} =
        return ()
      validateFieldDefaultValueType (DataDeclarationField _ fieldType (Just defaultValue)) = do
        evaluatedType <- evaluateType environment defaultValue
        Monad.unless (fieldType == evaluatedType) . Except.throwError $
          TypeMismatchError
            { expectedType = Type.getTypeName fieldType,
              actualType = Type.getTypeName evaluatedType,
              context = "evaluate data declaration default value expression"
            }
        return ()
evaluateType
  environment
  EnumDeclarationExpression {enumDeclarationName, enumDeclarationVariants, enumDeclarationBody} = do
    let extendedEnvironment = Environment.with enumDeclarationName declarationType environment
    let bodyEnvironment = List.foldr extendEnvironment extendedEnvironment enumDeclarationVariants
    evaluateType bodyEnvironment enumDeclarationBody
    where
      declarationType = NamedType enumDeclarationName

      extendEnvironment variant =
        Environment.with (Expression.enumVariantName variant) declarationType
evaluateType environment DataInstanceExpression {instanceName, instanceFields} = do
  instanceType <- TypeEnvironment.getType environment instanceName
  evaluateDataType instanceType
  where
    instanceNamedType = NamedType instanceName

    evaluateDataType (DataType typeFields) = do
      evaluatedInstanceFields <- Map.fromList <$> Monad.mapM evaluateInstanceField instanceFields
      Monad.mapM_ (validateField evaluatedInstanceFields) typeFields
      return instanceNamedType
    evaluateDataType instanceType =
      Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName instanceNamedType,
            actualType = Type.getTypeName instanceType,
            context = "evaluate data instance expression"
          }

    evaluateInstanceField DataInstanceField {Expression.fieldName, fieldValue} = do
      evaluatedType <- evaluateType environment fieldValue
      return (fieldName, evaluatedType)

    validateField :: Map Name PhallType -> DataTypeField -> Result ()
    validateField fields DataTypeField {Type.fieldName, Type.fieldType, fieldHasDefault} = do
      case Map.lookup fieldName fields of
        Nothing | fieldHasDefault -> return ()
        Nothing -> Except.throwError $ FieldInstanceNotFoundError {Error.fieldName = fieldName}
        Just instanceFieldType -> do
          Monad.unless (fieldType == instanceFieldType) . Except.throwError $
            TypeMismatchError
              { expectedType = Type.getTypeName fieldType,
                actualType = Type.getTypeName instanceFieldType,
                context = "evaluate data instance expression"
              }
          return ()
evaluateType environment InternalCallExpression {calleeName, arguments} = do
  argumentsTypes <- Monad.mapM (evaluateType environment) arguments
  (callArgumentsTypes, callReturnType) <- Internal.internalCallType calleeName
  Monad.unless (argumentsTypes == callArgumentsTypes) . Except.throwError $
    TypeMismatchError
      { expectedType = "?",
        actualType = "?",
        context = "evaluate internal call expression"
      }
  return callReturnType
evaluateType environment LambdaExpression {parameter, body, Expression.bodyType} = do
  let parameterType = evaluateParameterType $ Expression.parameterType parameter
  let parameterName = Expression.parameterName parameter
  let bodyEnvironment = Environment.with parameterName parameterType environment
  evaluatedBodyType <- evaluateType bodyEnvironment body
  case bodyType of
    UnknownType ->
      return $ LambdaType parameterType evaluatedBodyType
    _
      | bodyType == evaluatedBodyType ->
        return $ LambdaType parameterType evaluatedBodyType
    _ ->
      Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName bodyType,
            actualType = Type.getTypeName evaluatedBodyType,
            context = "evaluate lambda expression"
          }
  where
    evaluateParameterType UnknownType = AnyType -- TODO: get from body context
    evaluateParameterType other = other
evaluateType environment ApplicationExpression {function, argument} = do
  functionType <- evaluateType environment function
  evaluateFunctionType functionType
  where
    evaluateFunctionType LambdaType {Type.parameterType, Type.bodyType} = do
      argumentType <- evaluateType environment argument
      Monad.unless (parameterType == argumentType) . Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName parameterType,
            actualType = Type.getTypeName argumentType,
            context = "evaluate application expression"
          }
      return bodyType
    -- TODO: remove temporary fix
    evaluateFunctionType AnyType = return AnyType
    --
    evaluateFunctionType functionType =
      Except.throwError $
        TypeMismatchError
          { expectedType = "Lambda",
            actualType = Type.getTypeName functionType,
            context = "evaluate application expression: " <> Text.pack (show function)
          }
evaluateType environment (TupleExpression tuple) = do
  elementsTypes <- Monad.mapM (evaluateType environment) tuple
  return (TupleType elementsTypes)
evaluateType environment (ListExpression list) = do
  elementsTypes <- Monad.mapM (evaluateType environment) list
  let listType = getListType elementsTypes
  Monad.unless (Prelude.all (listType ==) elementsTypes) . Except.throwError $
    TypeMismatchError
      { expectedType = Type.getTypeName listType,
        actualType =
          "[" <> (Text.intercalate "," . Prelude.map Type.getTypeName $ elementsTypes) <> "]",
        context = "evaluate list expression"
      }
  return (ListType listType)
  where
    getListType [] = AnyType
    getListType types = Prelude.head types
evaluateType environment ConditionalExpression {condition, positive, negative} = do
  conditionType <- evaluateType environment condition
  evaluateConditionType conditionType
  where
    evaluateConditionType (ConstantType BooleanType) = do
      positiveType <- evaluateType environment positive
      negativeType <- evaluateType environment negative
      Monad.unless (positiveType == negativeType) . Except.throwError $
        TypeMismatchError
          { expectedType = Type.getTypeName positiveType,
            actualType = Type.getTypeName negativeType,
            context = "evaluate conditional expression"
          }

      return positiveType
    evaluateConditionType conditionType =
      Except.throwError $
        TypeMismatchError
          { expectedType = "Boolean",
            actualType = Type.getTypeName conditionType,
            context = "evalueate conditional expression"
          }
evaluateType _ (ConstantExpression constant) =
  return (evaluateConstantType constant)
evaluateType environment (VariableExpression name) = do
  TypeEnvironment.getType environment name

evaluateConstantType :: PhallConstant -> PhallType
evaluateConstantType UnitConstant = ConstantType UnitType
evaluateConstantType NoneConstant = ConstantType NoneType
evaluateConstantType (BooleanConstant _) = ConstantType BooleanType
evaluateConstantType (IntegerConstant _) = ConstantType IntegerType
evaluateConstantType (FloatConstant _) = ConstantType FloatType
evaluateConstantType (CharConstant _) = ConstantType CharType
evaluateConstantType (StringConstant _) = ConstantType StringType
