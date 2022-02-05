{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Evaluator.Builtin where

import Control.Monad.Except as Except
import Error (EvaluatorError (..))
import Evaluator.BuiltinTH
import Evaluator.PhallValue (ClosureInner (..), PhallValue (..))

add :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
add = $(makeArithmeticOperation '(+) '(+))

subtract :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
subtract = $(makeArithmeticOperation '(-) '(-))

multiply :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
multiply = $(makeArithmeticOperation '(*) '(*))

divide :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
divide = $(makeArithmeticOperation 'quot '(/))

isEqual :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
isEqual first second = return . BooleanValue $ first == second

fold :: PhallValue -> PhallValue -> PhallValue -> Except EvaluatorError PhallValue
fold closure firstElement (ListValue list) = do
  foldr folder (return firstElement) list
  where
    folder element exceptAccumulator = do
      accumulator <- exceptAccumulator
      elementClosure <- unwrapClosure closure
      innerClosure <- elementClosure element
      accumulatorClosure <- unwrapClosure innerClosure
      accumulatorClosure accumulator
fold _ _ _ =
  Except.throwError $ InvalidTypeError "(a -> b -> b) -> b -> [a] -> b" ""

unwrapClosure ::
  PhallValue -> Except EvaluatorError (PhallValue -> Except EvaluatorError PhallValue)
unwrapClosure (ClosureValue (ClosureInner closure)) = return closure
unwrapClosure _ = Except.throwError $ InvalidTypeError "Closure" ""
