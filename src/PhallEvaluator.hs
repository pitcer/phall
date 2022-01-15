module PhallEvaluator where

import PhallParser
  ( PhallConstant
      ( BooleanConstant,
        CharConstant,
        FloatConstant,
        IntegerConstant,
        StringConstant
      ),
    PhallExpression (ConstantExpression, VariableExpression),
  )

data PhallValue
  = BooleanValue Bool
  | IntegerValue Integer
  | FloatValue Double
  | CharValue Char
  | StringValue String

evaluate :: PhallExpression -> PhallValue
evaluate (ConstantExpression constant) = evaluateConstant constant
evaluate (VariableExpression name) = StringValue $ "Variable: " ++ name

evaluateConstant :: PhallConstant -> PhallValue
evaluateConstant (BooleanConstant boolean) = BooleanValue boolean
evaluateConstant (IntegerConstant integer) = IntegerValue integer
evaluateConstant (FloatConstant float) = FloatValue float
evaluateConstant (CharConstant char) = CharValue char
evaluateConstant (StringConstant string) = StringValue string
