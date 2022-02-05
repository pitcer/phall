module Evaluator.PhallValue where

import Error (EvaluatorError)
import Control.Monad.Except (Except)
import Data.Text.Internal.Lazy (Text)

data PhallValue
  = BooleanValue Bool
  | IntegerValue Integer
  | FloatValue Double
  | CharValue Char
  | StringValue Text
  | ListValue [PhallValue]
  | ClosureValue ClosureInner
  deriving (Show)

newtype ClosureInner
  = ClosureInner (PhallValue -> Except EvaluatorError PhallValue)

instance Show ClosureInner where
  show _ = "Closure"
