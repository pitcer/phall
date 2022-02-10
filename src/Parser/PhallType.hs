{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallType where

import Data.Text.Lazy as Text
import Lexer.Symbol as Symbol

data PhallType
  = AnyType
  | ConstantType PhallConstantType
  | NamedType Name
  | ListType PhallType
  | OptionType PhallType
  | LambdaType
      { parameterType :: PhallType,
        bodyType :: PhallType
      }
  | DataType [DataTypeField]
  deriving (Show)

data PhallConstantType
  = BooleanType
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

type Name = Text

instance Eq PhallType where
  AnyType == _ = True
  _ == AnyType = True
  ConstantType first == ConstantType second = first == second
  NamedType first == NamedType second = first == second
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
getTypeName (NamedType name) = name
getTypeName (ListType listType) =
  enumName LeftSquareBracket <> getTypeName listType <> enumName RightSquareBracket
getTypeName (OptionType optionType) =
  getTypeName optionType <> enumName QuestionMark
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
