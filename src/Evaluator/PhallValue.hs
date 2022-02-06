module Evaluator.PhallValue where

import Control.Monad.Except as Except
import Data.Text.Internal.Lazy (Text)
import Error (EvaluatorError)
import Data.Map as Map

data PhallValue
  = BooleanValue Bool
  | IntegerValue Integer
  | FloatValue Double
  | CharValue Char
  | StringValue Text
  | ListValue [PhallValue]
  | ClosureValue ClosureInner
  | DataValue (Map Text PhallValue)
  deriving (Show, Eq)

newtype ClosureInner
  = ClosureInner (PhallValue -> Except EvaluatorError PhallValue)

instance Show ClosureInner where
  show _ = "Closure"

instance Eq ClosureInner where
  _ == _ = False
