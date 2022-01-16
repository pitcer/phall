{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PhallEvaluator where

import Control.Monad.Except (Except)
import qualified Control.Monad.Except as Except (throwError)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text (concat)
import Error (EvaluatorError (..))
import PhallParser (PhallConstant (..), PhallExpression (..))

data PhallValue
  = BooleanValue Bool
  | IntegerValue Integer
  | FloatValue Double
  | CharValue Char
  | StringValue Text
  deriving (Show)

evaluate :: PhallExpression -> Except EvaluatorError PhallValue
evaluate LambdaExpression {parameter, body} = return $ StringValue "Lambda"
evaluate ApplicationExpression {function, argument} = return $ StringValue "Application"
evaluate ConditionalExpression {condition, positive, negative} = do
  value <- evaluate condition
  case value of
    BooleanValue booleanValue ->
      if booleanValue
        then evaluate positive
        else evaluate negative
    _ -> Except.throwError $ InvalidTypeError "Boolean"
evaluate (ConstantExpression constant) = return $ evaluateConstant constant
evaluate (VariableExpression name) = return . StringValue $ Text.concat ["Variable: ", name]

evaluateConstant :: PhallConstant -> PhallValue
evaluateConstant (BooleanConstant boolean) = BooleanValue boolean
evaluateConstant (IntegerConstant integer) = IntegerValue integer
evaluateConstant (FloatConstant float) = FloatValue float
evaluateConstant (CharConstant char) = CharValue char
evaluateConstant (StringConstant string) = StringValue string
