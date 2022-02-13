{-# LANGUAGE NamedFieldPuns #-}

module Evaluator.PhallValue where

import Control.Monad.Except as Except
import Data.Map as Map
import Data.Text.Internal.Lazy (Text)
import Environment
import Error (EvaluatorError)
import Parser.PhallType as Type

data PhallValue
  = UnitValue
  | BooleanValue Bool
  | IntegerValue Integer
  | FloatValue Double
  | CharValue Char
  | StringValue Text
  | TupleValue [PhallValue]
  | ListValue [PhallValue]
  | ClosureValue ClosureInner
  | DataValue (Map Text PhallValue)
  | ExportBundleValue (Environment PhallValue)
  deriving (Show, Eq)

newtype ClosureInner
  = ClosureInner (PhallValue -> Except EvaluatorError PhallValue)

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
  TupleType $ Prelude.map getValueType tuple
getValueType (ListValue list) =
  ListType . getValueType . Prelude.head $ list
getValueType (ClosureValue _) =
  LambdaType {parameterType = AnyType, bodyType = AnyType}
getValueType (DataValue dataValue) =
  DataType . Prelude.map toFieldType . Map.toList $ dataValue
  where
    toFieldType (fieldName, fieldType) =
      DataTypeField {fieldName, fieldType = getValueType fieldType}
getValueType (ExportBundleValue _) = UnknownType
