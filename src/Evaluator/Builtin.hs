{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Evaluator.Builtin where

import Control.Monad.Except as Except
import Error (EvaluatorError (..))
import Evaluator.BuiltinTH
import Evaluator.PhallValue (ClosureInner (..), PhallValue (..))
import Parser.PhallType

add :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
add = $(makeArithmeticOperation '(+) '(+))

subtract :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
subtract = $(makeArithmeticOperation '(-) '(-))

multiply :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
multiply = $(makeArithmeticOperation '(*) '(*))

divide :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
divide = $(makeArithmeticOperation 'quot '(/))

-- TODO: also allow floats
arithmeticOperationType :: PhallType
arithmeticOperationType = createNestedLambdas [IntegerType, IntegerType, IntegerType]

--  LambdaType
--    { parameterType = IntegerType,
--      bodyType =
--        LambdaType
--          { parameterType = IntegerType,
--            bodyType = IntegerType
--          }
--    }

isEqual :: PhallValue -> PhallValue -> Except EvaluatorError PhallValue
isEqual first second = return . BooleanValue $ first == second

isEqualType :: PhallType
isEqualType =
  --createNestedLambdas [AnyType, AnyType, BooleanType]
  LambdaType
    { parameterType = AnyType,
      bodyType =
        LambdaType
          { parameterType = AnyType,
            bodyType = BooleanType
          }
    }

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

foldType :: PhallType
foldType =
  createNestedLambdas
    [ createNestedLambdas [AnyType, AnyType, AnyType],
      AnyType,
      ListType AnyType,
      AnyType
    ]

unwrapClosure ::
  PhallValue -> Except EvaluatorError (PhallValue -> Except EvaluatorError PhallValue)
unwrapClosure (ClosureValue (ClosureInner closure)) = return closure
unwrapClosure _ = Except.throwError $ InvalidTypeError "Closure" ""

createNestedLambdas :: [PhallType] -> PhallType
createNestedLambdas =
  foldr1
    ( \element accumulator ->
        LambdaType
          { parameterType = element,
            bodyType = accumulator
          }
    )
