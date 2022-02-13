{-# LANGUAGE OverloadedStrings #-}

module Internal.InternalType where

import Common
import Control.Monad.Except as Except
import Error
import Parser.PhallType

type InternalCallType = ([PhallType], PhallType)

internalCallType :: Name -> Result InternalCallType
internalCallType "add" = return arithmeticOperationType
internalCallType "sub" = return arithmeticOperationType
internalCallType "mul" = return arithmeticOperationType
internalCallType "div" = return arithmeticOperationType
internalCallType "fold" =
  return ([createNestedLambdas [AnyType, AnyType, AnyType], AnyType, ListType AnyType], AnyType)
internalCallType "cons" =
  return ([AnyType, ListType AnyType], ListType AnyType)
internalCallType "isEqual" =
  return ([AnyType, AnyType], ConstantType BooleanType)
internalCallType name = Except.throwError $ TypeNotFoundError name

arithmeticOperationType :: InternalCallType
arithmeticOperationType = ([AnyType, AnyType], AnyType)

createNestedLambdas :: [PhallType] -> PhallType
createNestedLambdas =
  foldr1
    ( \element accumulator ->
        LambdaType
          { parameterType = element,
            bodyType = accumulator
          }
    )
