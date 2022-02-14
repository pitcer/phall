{-# LANGUAGE NamedFieldPuns #-}

module Evaluator.PhallValue where

import qualified Data.List as List
import Data.Text.Lazy as Text
import Environment
import Error
import Parser.PhallType as Type
import Text.JSON as Json

data PhallValue
  = UnitValue
  | BooleanValue Bool
  | IntegerValue Integer
  | FloatValue Double
  | CharValue Char
  | StringValue String
  | TupleValue [PhallValue]
  | ListValue [PhallValue]
  | ClosureValue ClosureInner
  | DataValue [(Text, PhallValue)]
  | EnumValue PhallValue
  | ExportBundleValue (Environment PhallValue)
  deriving (Show, Eq)

newtype ClosureInner
  = ClosureInner (PhallValue -> Error.Result PhallValue)

instance Show ClosureInner where
  show _ = "Closure"

instance Eq ClosureInner where
  _ == _ = False

getValueType :: PhallValue -> PhallType
getValueType UnitValue = ConstantType UnitType
getValueType (BooleanValue _) = ConstantType BooleanType
getValueType (IntegerValue _) = ConstantType IntegerType
getValueType (FloatValue _) = ConstantType FloatType
getValueType (CharValue _) = ConstantType CharType
getValueType (StringValue _) = ConstantType StringType
getValueType (TupleValue tuple) =
  TupleType $ List.map getValueType tuple
getValueType (ListValue []) =
  ListType AnyType
getValueType (ListValue list) =
  ListType . getValueType . Prelude.head $ list
getValueType (ClosureValue _) =
  LambdaType {parameterType = AnyType, bodyType = AnyType}
getValueType (DataValue dataValue) =
  DataType . List.map toFieldType $ dataValue
  where
    toFieldType (fieldName, fieldType) =
      DataTypeField {fieldName, fieldType = getValueType fieldType}
getValueType (EnumValue _) = UnknownType
getValueType (ExportBundleValue _) = UnknownType

instance JSON PhallValue where
  readJSON _ =
    Error "Reading JSON to Phall is not supported"

  showJSON UnitValue = Json.showJSON ()
  showJSON (BooleanValue value) = Json.showJSON value
  showJSON (IntegerValue value) = Json.showJSON value
  showJSON (FloatValue value) = Json.showJSON value
  showJSON (CharValue value) = Json.showJSON value
  showJSON (StringValue value) = Json.showJSON value
  showJSON (TupleValue tuple) = Json.showJSON tuple
  showJSON (ListValue list) = Json.showJSON list
  showJSON (ClosureValue _) = JSNull
  showJSON (DataValue fields) =
    Json.makeObj . List.map mapField $ fields
    where
      mapField (fieldName, fieldValue) =
        (Text.unpack fieldName, showJSON fieldValue)
  showJSON (EnumValue value) = Json.showJSON value
  showJSON (ExportBundleValue _) =
    error "Trying to convert export bundle to JSON"
