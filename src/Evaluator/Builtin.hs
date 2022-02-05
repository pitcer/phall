{-# LANGUAGE OverloadedStrings #-}

module Evaluator.Builtin where

import Control.Monad.Except (Except)
import qualified Control.Monad.Except as Except (throwError)
import Error (EvaluatorError (..))
import Evaluator.PhallValue (ClosureInner (..), PhallValue (..))

add :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
add (IntegerValue first) (IntegerValue second) =
  return . IntegerValue $ first + second
add (FloatValue first) (FloatValue second) =
  return . FloatValue $ first + second
add _ _ =
  Except.throwError $ InvalidTypeError "Integer -> Integer -> Integer | Float -> Float -> Float" ""

fold :: PhallValue -> PhallValue -> PhallValue -> Except EvaluatorError PhallValue
fold closure firstElement (ListValue list) = do
  foldr folder (return firstElement) list
  where
    folder element exceptAccumulator = do
      accumulator <- exceptAccumulator
      elementClosure <- unwrapClosure closure
      cl2 <- elementClosure element
      accumulatorClosure <- unwrapClosure cl2
      accumulatorClosure accumulator
fold _ _ _ =
  Except.throwError $ InvalidTypeError "(a -> b -> b) -> b -> [a] -> b" ""

unwrapClosure ::
  PhallValue -> Except EvaluatorError (PhallValue -> Except EvaluatorError PhallValue)
unwrapClosure (ClosureValue (ClosureInner closure)) = return closure
unwrapClosure _ = Except.throwError $ InvalidTypeError "Closure" ""
