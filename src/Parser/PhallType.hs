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
  | BooleanType
  | IntegerType
  | FloatType
  | CharType
  | StringType
  deriving (Show, Eq)

data DataTypeField = DataTypeField
  { fieldName :: Name,
    fieldType :: PhallType
  }
  deriving (Show, Eq)

instance Eq PhallType where
  AnyType == _ = True
  _ == AnyType = True
  UnknownType == UnknownType = True
  ConstantType first == ConstantType second = first == second
  NamedType first == NamedType second = first == second
  ListType first == ListType second = first == second
  TupleType first == TupleType second = first == second
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
getTypeName UnknownType = "unknown type"
getTypeName AnyType = enumName AnyTypeKeyword
getTypeName (ConstantType UnitType) = enumName UnitSymbol
getTypeName (ConstantType BooleanType) = enumName BooleanTypeKeyword
getTypeName (ConstantType IntegerType) = enumName IntegerTypeKeyword
getTypeName (ConstantType FloatType) = enumName FloatTypeKeyword
getTypeName (ConstantType CharType) = enumName CharTypeKeyword
getTypeName (ConstantType StringType) = enumName StringTypeKeyword
getTypeName (NamedType name) = name
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
  "{" <> Text.intercalate ", " fieldsText <> "}"
  where
    fieldsText =
      Prelude.map
        (\DataTypeField {fieldName, fieldType} -> fieldName <> ": " <> getTypeName fieldType)
        fields
getTypeName (ExportBundleType environment) =
  "(export " <> Text.intercalate ", " (Environment.names environment) <> ")"
