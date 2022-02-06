{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PhallType where

import Data.Text.Lazy as Text
import Lexer.Symbol as Symbol

data PhallType
  = AnyType
  | BooleanType
  | IntegerType
  | FloatType
  | CharType
  | StringType
  | ListType PhallType
  | OptionType PhallType
  | LambdaType
      { parameterType :: PhallType,
        bodyType :: PhallType
      }
  deriving (Show, Eq)

typesEqual :: PhallType -> PhallType -> Bool
typesEqual AnyType _ = True
typesEqual _ AnyType = True
typesEqual first second = first == second

fromTypeKeyword :: TypeKeyword -> PhallType
fromTypeKeyword AnyTypeKeyword = AnyType
fromTypeKeyword BooleanTypeKeyword = BooleanType
fromTypeKeyword IntegerTypeKeyword = IntegerType
fromTypeKeyword FloatTypeKeyword = FloatType
fromTypeKeyword CharTypeKeyword = CharType
fromTypeKeyword StringTypeKeyword = StringType

getTypeName :: PhallType -> Text
getTypeName AnyType = enumName AnyTypeKeyword
getTypeName BooleanType = enumName BooleanTypeKeyword
getTypeName IntegerType = enumName IntegerTypeKeyword
getTypeName FloatType = enumName FloatTypeKeyword
getTypeName CharType = enumName CharTypeKeyword
getTypeName StringType = enumName StringTypeKeyword
getTypeName (ListType listType) =
  Text.concat [enumName LeftSquareBracket, getTypeName listType, enumName RightSquareBracket]
getTypeName (OptionType optionType) =
  Text.concat [getTypeName optionType, enumName QuestionMark]
getTypeName LambdaType {parameterType, bodyType} =
  Text.concat
    [ "(",
      getTypeName parameterType,
      " ",
      enumName RightArrowSymbol,
      " ",
      getTypeName bodyType,
      ")"
    ]
