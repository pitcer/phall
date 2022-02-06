{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallType where

import Data.Text.Lazy as Text
import Lexer.Symbol as Symbol

data PhallType
  = AnyType
  | ConstantType PhallConstantType
  | ListType PhallType
  | OptionType PhallType
  | LambdaType
      { parameterType :: PhallType,
        bodyType :: PhallType
      }
  deriving (Show)

data PhallConstantType
  = BooleanType
  | IntegerType
  | FloatType
  | CharType
  | StringType
  deriving (Show, Eq)

instance Eq PhallType where
  AnyType == _ = True
  _ == AnyType = True
  ConstantType first == ConstantType second = first == second
  ListType first == ListType second = first == second
  OptionType first == OptionType second = first == second
  (==)
    LambdaType
      { parameterType = firstParameter,
        bodyType = firstBody
      }
    LambdaType
      { parameterType = secondParameter,
        bodyType = secondBody
      } = firstParameter == secondParameter && firstBody == secondBody
  _ == _ = False

fromTypeKeyword :: TypeKeyword -> PhallType
fromTypeKeyword AnyTypeKeyword = AnyType
fromTypeKeyword BooleanTypeKeyword = ConstantType BooleanType
fromTypeKeyword IntegerTypeKeyword = ConstantType IntegerType
fromTypeKeyword FloatTypeKeyword = ConstantType FloatType
fromTypeKeyword CharTypeKeyword = ConstantType CharType
fromTypeKeyword StringTypeKeyword = ConstantType StringType

getTypeName :: PhallType -> Text
getTypeName AnyType = enumName AnyTypeKeyword
getTypeName (ConstantType BooleanType) = enumName BooleanTypeKeyword
getTypeName (ConstantType IntegerType) = enumName IntegerTypeKeyword
getTypeName (ConstantType FloatType) = enumName FloatTypeKeyword
getTypeName (ConstantType CharType) = enumName CharTypeKeyword
getTypeName (ConstantType StringType) = enumName StringTypeKeyword
getTypeName (ListType listType) =
  enumName LeftSquareBracket <> getTypeName listType <> enumName RightSquareBracket
getTypeName (OptionType optionType) =
  getTypeName optionType <> enumName QuestionMark
getTypeName LambdaType {parameterType, bodyType} =
  "(" <> getTypeName parameterType <> " " <> enumName RightArrowSymbol <> " " <> getTypeName bodyType <> ")"
