{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallType where

import Common
import Data.Text.Lazy as Text
import Environment
import Lexer.Symbol as Symbol

data PhallType
  = UnknownType
  | AnyType
  | ConstantType PhallConstantType
  | NamedType Name
  | ListType PhallType
  | OptionType PhallType
  | LambdaType
      { parameterType :: PhallType,
        bodyType :: PhallType
      }
  | TupleType [PhallType]
  | DataType [DataTypeField]
  | ExportBundleType (Environment PhallType)
  deriving (Show)

data PhallConstantType
  = UnitType
  | NoneType
  | BooleanType
  | IntegerType
  | FloatType
  | CharType
  | StringType
  deriving (Show, Eq)

data DataTypeField = DataTypeField
  { fieldName :: Name,
    fieldType :: PhallType,
    fieldHasDefault :: Bool
  }
  deriving (Show, Eq)

instance Eq PhallType where
  OptionType _ == ConstantType NoneType = True
  ConstantType NoneType == OptionType _ = True
  _ == ConstantType NoneType = False
  ConstantType NoneType == _ = False
  AnyType == _ = True
  _ == AnyType = True
  UnknownType == UnknownType = True
  ConstantType first == ConstantType second = first == second
  NamedType first == NamedType second = first == second
  ListType first == ListType second = first == second
  TupleType first == TupleType second = first == second
  OptionType first == OptionType second = first == second
  OptionType first == second = first == second
  first == OptionType second = first == second
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
getTypeName UnknownType = "(unknown type)"
getTypeName AnyType = enumName AnyTypeKeyword
getTypeName (ConstantType UnitType) = enumName UnitSymbol
getTypeName (ConstantType NoneType) = enumName NoneKeyword
getTypeName (ConstantType BooleanType) = enumName BooleanTypeKeyword
getTypeName (ConstantType IntegerType) = enumName IntegerTypeKeyword
getTypeName (ConstantType FloatType) = enumName FloatTypeKeyword
getTypeName (ConstantType CharType) = enumName CharTypeKeyword
getTypeName (ConstantType StringType) = enumName StringTypeKeyword
getTypeName (NamedType name) = "(name: " <> name <> ")"
getTypeName (ListType listType) =
  enumName LeftSquareBracket <> getTypeName listType <> enumName RightSquareBracket
getTypeName (TupleType tupleType) =
  enumName LeftCurlyBracket
    <> Text.intercalate ", " (Prelude.map getTypeName tupleType)
    <> enumName RightCurlyBracket
getTypeName (OptionType optionType) =
  getTypeName optionType <> enumName QuestionMarkSymbol
getTypeName LambdaType {parameterType, bodyType} =
  "(" <> getTypeName parameterType <> " " <> enumName RightArrowSymbol <> " "
    <> getTypeName bodyType
    <> ")"
getTypeName (DataType fields) =
  "{" <> Text.intercalate ", " (Prelude.map stringifyField fields) <> "}"
  where
    stringifyField DataTypeField {fieldName, fieldType} =
      fieldName <> ": " <> getTypeName fieldType
getTypeName (ExportBundleType environment) =
  "(export " <> Text.intercalate ", " (Environment.names environment) <> ")"
