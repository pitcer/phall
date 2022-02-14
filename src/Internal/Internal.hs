{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Internal where

import Common
import Control.Monad.Except as Except
import qualified Data.List as List
import Data.Text.Lazy as Text
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
internalCall "fold" [accumulate, base, list] =
  callFold accumulate base list
internalCall "cons" [element, list] =
  callCons element list
internalCall "isEqual" [first, second] =
  return . BooleanValue $ first == second
internalCall name _ = Except.throwError $ UnknownInternalCall {callName = name}

callFold :: PhallValue -> PhallValue -> PhallValue -> Result PhallValue
callFold accumulate base (ListValue list) = do
  List.foldr folder (return base) list
  where
    folder element exceptAccumulator = do
      accumulator <- exceptAccumulator
      elementClosure <- unwrapClosure accumulate
      innerClosure <- elementClosure element
      accumulatorClosure <- unwrapClosure innerClosure
      accumulatorClosure accumulator
callFold accumulate base (StringValue string) = do
  let characterList = ListValue $ List.map CharValue string
  callFold accumulate base characterList
callFold accumulate base list =
  Except.throwError $
    TypeMismatchError
      { expectedType = "(a -> b -> b) -> b -> [a] | Str -> b",
        actualType =
          Text.concat
            [ Type.getTypeName $ Value.getValueType accumulate,
              " -> ",
              Type.getTypeName $ Value.getValueType base,
              " -> ",
              Type.getTypeName $ Value.getValueType list,
              " -> Any"
            ],
        context = "tried to evaluate @fold"
      }

callCons :: PhallValue -> PhallValue -> Result PhallValue
callCons element (ListValue list) =
  return . ListValue $ element : list
callCons (CharValue character) (StringValue string) =
  return . StringValue $ character : string
callCons element list =
  Except.throwError $
    TypeMismatchError
      { expectedType = "a | Char -> [a] | String -> [a] | String",
        actualType =
          Text.concat
            [ Type.getTypeName $ Value.getValueType element,
              " -> ",
              Type.getTypeName $ Value.getValueType list,
              " -> Any"
            ],
        context = "tried to evaluate @cons"
      }

unwrapClosure :: PhallValue -> Result (PhallValue -> Result PhallValue)
unwrapClosure (ClosureValue (ClosureInner closure)) = return closure
unwrapClosure value =
  Except.throwError $
    TypeMismatchError
      { expectedType = "Closure",
        actualType = Type.getTypeName $ Value.getValueType value,
        context = "tried to unwrap closure"
      }
