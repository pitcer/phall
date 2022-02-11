{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Internal where

import Control.Monad.Except as Except
import Error
import Evaluator.PhallValue as Value
import Internal.InternalTH as Internal
import Parser.PhallType as Type

internalCall :: Name -> [PhallValue] -> Except EvaluatorError PhallValue
internalCall "add" [first, second] =
  $(makeArithmeticOperation '(+) '(+)) first second
internalCall "sub" [first, second] =
  $(makeArithmeticOperation '(-) '(-)) first second
internalCall "mul" [first, second] =
  $(makeArithmeticOperation '(*) '(*)) first second
internalCall "div" [first, second] =
  $(makeArithmeticOperation 'quot '(/)) first second
internalCall "fold" [accumulate, base, ListValue list] = do
  foldr folder (return base) list
  where
    folder element exceptAccumulator = do
      accumulator <- exceptAccumulator
      elementClosure <- unwrapClosure accumulate
      innerClosure <- elementClosure element
      accumulatorClosure <- unwrapClosure innerClosure
      accumulatorClosure accumulator
internalCall "fold" [_, _, _] =
  Except.throwError $ InvalidTypeError "(a -> b -> b) -> b -> [a] -> b" ""
internalCall "cons" [element, ListValue list] =
  return . ListValue $ element : list
internalCall "cons" [_, _] =
  Except.throwError $ InvalidTypeError "a -> [a] -> [a]" ""
internalCall "isEqual" [first, second] =
  return . BooleanValue $ first == second
internalCall name _ = Except.throwError $ VariableNotFound name

unwrapClosure ::
  PhallValue -> Except EvaluatorError (PhallValue -> Except EvaluatorError PhallValue)
unwrapClosure (ClosureValue (ClosureInner closure)) = return closure
unwrapClosure value =
  Except.throwError . InvalidTypeError "Closure" . Type.getTypeName $
    Value.getValueType value
