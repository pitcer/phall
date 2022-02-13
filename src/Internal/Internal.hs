{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Internal where

import Common
import Control.Monad.Except as Except
import Error
import Evaluator.PhallValue as Value
import Internal.InternalTH as Internal
import Parser.PhallType as Type

internalCall :: Name -> [PhallValue] -> Result PhallValue
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
  Except.throwError $
    TypeMismatchError
      { expectedType = "(a -> b -> b) -> b -> [a] -> b",
        actualType = "",
        context = ""
      }
internalCall "cons" [element, ListValue list] =
  return . ListValue $ element : list
internalCall "cons" [_, _] =
  Except.throwError $
    TypeMismatchError
      { expectedType = "a -> [a] -> [a]",
        actualType = "",
        context = ""
      }
internalCall "isEqual" [first, second] =
  return . BooleanValue $ first == second
internalCall name _ = Except.throwError $ VariableNotFound name

unwrapClosure ::
  PhallValue -> Result (PhallValue -> Result PhallValue)
unwrapClosure (ClosureValue (ClosureInner closure)) = return closure
unwrapClosure value =
  Except.throwError $
    TypeMismatchError
      { expectedType = "Closure",
        actualType = Type.getTypeName $ Value.getValueType value,
        context = ""
      }
