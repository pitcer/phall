{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Text.Lazy (Text)
import Error
import Parser.PhallExpression as Expression
import qualified Parser.PhallParser as Parser (parse)
import Parser.PhallType
import Test.Hspec
import qualified Text.Megaparsec as Megaparsec

spec :: Spec
spec = do
  describe "PhallParser.parse" $ do
    it "returns 1 integer contant from '1'" $ do
      expression <- parse "1"
      expression `shouldBe` Right (ConstantExpression $ IntegerConstant 1)

    it "returns 1 integer contant from '(1)'" $ do
      expression <- parse "(1)"
      expression `shouldBe` Right (ConstantExpression $ IntegerConstant 1)

    it "returns double application to variable 'add' from '((add 1) 2)'" $ do
      expression <- parse "((add 1) 2)"
      expression
        `shouldBe` Right
          ( ApplicationExpression
              { function =
                  ApplicationExpression
                    { function = VariableExpression "add",
                      argument = ConstantExpression $ IntegerConstant 1
                    },
                argument = ConstantExpression $ IntegerConstant 2
              }
          )

    it "returns double application to variable 'add' from '(add 1) 2'" $ do
      expression <- parse "(add 1) 2"
      expression
        `shouldBe` Right
          ( ApplicationExpression
              { function =
                  ApplicationExpression
                    { function = VariableExpression "add",
                      argument = ConstantExpression $ IntegerConstant 1
                    },
                argument = ConstantExpression $ IntegerConstant 2
              }
          )

    it "returns double application to variable 'add' from 'add 1 2'" $ do
      expression <- parse "add 1 2"
      expression
        `shouldBe` Right
          ( ApplicationExpression
              { function =
                  ApplicationExpression
                    { function = VariableExpression "add",
                      argument = ConstantExpression $ IntegerConstant 1
                    },
                argument = ConstantExpression $ IntegerConstant 2
              }
          )

    it "returns double application to variable 'add' from '((add a) 2)'" $ do
      expression <- parse "((add a) 2)"
      expression
        `shouldBe` Right
          ( ApplicationExpression
              { function =
                  ApplicationExpression
                    { function = VariableExpression "add",
                      argument = VariableExpression "a"
                    },
                argument = ConstantExpression $ IntegerConstant 2
              }
          )

    it "returns double application to variable 'add' from '(add a) 2'" $ do
      expression <- parse "(add a) 2"
      expression
        `shouldBe` Right
          ( ApplicationExpression
              { function =
                  ApplicationExpression
                    { function = VariableExpression "add",
                      argument = VariableExpression "a"
                    },
                argument = ConstantExpression $ IntegerConstant 2
              }
          )

    it "returns double application to variable 'add' from 'add a 2'" $ do
      expression <- parse "add a 2"
      expression
        `shouldBe` Right
          ( ApplicationExpression
              { function =
                  ApplicationExpression
                    { function = VariableExpression "add",
                      argument = VariableExpression "a"
                    },
                argument = ConstantExpression $ IntegerConstant 2
              }
          )

    it "parsers application inside lambda 'a b -> f a b'" $ do
      expression <- parse "a b -> f a b"
      expression
        `shouldBe` Right
          ( LambdaExpression
              { parameter =
                  LambdaParameter
                    { parameterName = "a",
                      Expression.parameterType = UnknownType
                    },
                body =
                  LambdaExpression
                    { parameter =
                        LambdaParameter
                          { parameterName = "b",
                            Expression.parameterType = UnknownType
                          },
                      body =
                        ApplicationExpression
                          { function =
                              ApplicationExpression
                                { function = VariableExpression "f",
                                  argument = VariableExpression "a"
                                },
                            argument = VariableExpression "b"
                          },
                      Expression.bodyType = UnknownType
                    },
                Expression.bodyType = UnknownType
              }
          )

parse :: Text -> IO (Either ParserError PhallExpression)
parse = Megaparsec.runParserT Parser.parse "hspec"
