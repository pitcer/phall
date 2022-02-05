{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Evaluator.BuiltinTH where

import Control.Monad.Except as Except
import Error (EvaluatorError (..))
import Evaluator.PhallValue (PhallValue (..))
import Language.Haskell.TH

makeArithmeticOperation :: Name -> Name -> ExpQ
makeArithmeticOperation integerOperation floatOperation =
  [|
    \firstArgument secondArgument -> case (firstArgument, secondArgument) of
      (IntegerValue first, IntegerValue second) ->
        return . IntegerValue $ $(varE integerOperation) first second
      (FloatValue first, FloatValue second) ->
        return . FloatValue $ $(varE floatOperation) first second
      _ ->
        Except.throwError $
          InvalidTypeError "Integer -> Integer -> Integer | Float -> Float -> Float" ""
    |]
