{-# LANGUAGE NamedFieldPuns #-}

module Parser.PhallExpression where

import Common
import Data.Text.Lazy as Text
import FullSet
import Parser.PhallType as Type

data PhallExpression
  = ImportExpression
      { importSource :: PhallExpression,
        importedItems :: FullSet Name,
        importBody :: PhallExpression
      }
  | ExportExpression (FullSet Name)
  | TypeDeclarationExpression
      { typeDeclarationName :: Name,
        typeDeclarationType :: PhallType,
        typeDeclarationBody :: PhallExpression
      }
  | DataDeclarationExpression
      { declarationName :: Name,
        declarationFields :: [DataDeclarationField],
        declarationBody :: PhallExpression
      }
  | EnumDeclarationExpression
      { enumDeclarationName :: Name,
        enumDeclarationVariants :: [EnumVariant],
        enumDeclarationBody :: PhallExpression
      }
  | DataInstanceExpression
      { instanceName :: Name,
        instanceFields :: [DataInstanceField]
      }
  | InternalCallExpression
      { calleeName :: Name,
        arguments :: [PhallExpression]
      }
  | LambdaExpression
      { parameter :: LambdaParameter,
        body :: PhallExpression,
        bodyType :: PhallType
      }
  | ApplicationExpression
      { function :: PhallExpression,
        argument :: PhallExpression
      }
  | ConditionalExpression
      { condition :: PhallExpression,
        positive :: PhallExpression,
        negative :: PhallExpression
      }
  | TupleExpression [PhallExpression]
  | ListExpression [PhallExpression]
  | ConstantExpression PhallConstant
  | VariableExpression Name
  deriving (Show, Eq)

data LambdaParameter = LambdaParameter
  { parameterName :: Name,
    parameterType :: PhallType
  }
  deriving (Show, Eq)

data DataDeclarationField = DataDeclarationField
  { declarationFieldName :: Name,
    declarationFieldType :: PhallType,
    declarationFieldDefaultValue :: Maybe PhallExpression
  }
  deriving (Show, Eq)

toTypeField :: DataDeclarationField -> DataTypeField
toTypeField (DataDeclarationField fieldName fieldType Nothing) =
  DataTypeField {Type.fieldName, Type.fieldType, Type.fieldHasDefault = False}
toTypeField (DataDeclarationField fieldName fieldType (Just _)) =
  DataTypeField {Type.fieldName, Type.fieldType, Type.fieldHasDefault = True}

data EnumVariant = EnumVariant
  { enumVariantName :: Name,
    enumVariantValue :: PhallExpression
  }
  deriving (Show, Eq)

data DataInstanceField = DataInstanceField
  { fieldName :: Name,
    fieldValue :: PhallExpression
  }
  deriving (Show, Eq)

data PhallConstant
  = UnitConstant
  | NoneConstant
  | BooleanConstant Bool
  | IntegerConstant Integer
  | FloatConstant Double
  | CharConstant Char
  | StringConstant Text
  deriving (Show, Eq)
