{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Text.Lazy (Text)
import Error
import Parser.PhallExpression
import qualified Parser.PhallParser as Parser (parse)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.Megaparsec as Megaparsec (parse)

spec :: Spec
spec = do
  describe "PhallParser.parse" $ do
    it "returns 1 integer contant from '1'" $ do
      parse "1" `shouldBe` Right (ConstantExpression $ IntegerConstant 1)

    it "returns 1 integer contant from '(1)'" $ do
      parse "(1)" `shouldBe` Right (ConstantExpression $ IntegerConstant 1)

    it "returns double application to variable 'add' from '((add 1) 2)'" $ do
      parse "((add 1) 2)"
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
      parse "(add 1) 2"
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
      parse "add 1 2"
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
      parse "((add a) 2)"
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
      parse "(add a) 2"
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
      parse "add a 2"
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
      parse "a b -> f a b"
        `shouldBe` Right
          ( LambdaExpression
              { parameter = "a",
                maybeParameterType = Nothing,
                body =
                  LambdaExpression
                    { parameter = "b",
                      maybeParameterType = Nothing,
                      body =
                        ApplicationExpression
                          { function =
                              ApplicationExpression
                                { function = VariableExpression "f",
                                  argument = VariableExpression "a"
                                },
                            argument = VariableExpression "b"
                          },
                      maybeBodyType = Nothing
                    },
                maybeBodyType = Nothing
              }
          )

parse :: Text -> Either ParserError PhallExpression
parse = Megaparsec.parse Parser.parse "hspec"
