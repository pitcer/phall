module Interpreter (runInterpreter) where

import Common
import Control.Monad.Except as Except
import Data.Text.Lazy.IO as TextIO
import Error
import Evaluator.PhallEvaluator as Evaluator
import Parser.PhallExpression
import Parser.PhallParser as Parser
import Parser.PhallType as Type
import Text.Megaparsec as Megaparsec
import Text.Pretty.Simple as PrettySimple
import TypeEvaluator.TypeEvaluator as TypeEvaluator

runInterpreter :: IO ()
runInterpreter = do
  result <- Except.runExceptT interpret
  case result of
    Left (ParserError errorBundle) -> Prelude.putStrLn $ Megaparsec.errorBundlePretty errorBundle
    Left (TypeError typeError) -> print $ Error.message typeError
    Left (EvaluatorError evaluatorError) -> print $ Error.message evaluatorError
    Right _ -> return ()

interpret :: ExceptIO PhallError ()
interpret = do
  expression <-
    Except.withExceptT ParserError $ parseFromFile Parser.parse "playground/test.phall"
  Except.liftIO $ PrettySimple.pPrint expression
  (typedExpression, expressionType) <-
    Except.liftEither
      . Except.runExcept
      . Except.withExceptT TypeError
      $ TypeEvaluator.evaluate expression
  Except.liftIO $ PrettySimple.pPrint typedExpression
  Except.liftIO . TextIO.putStrLn $ Type.getTypeName expressionType
  value <-
    Except.liftEither
      . Except.runExcept
      . Except.withExceptT EvaluatorError
      $ Evaluator.evaluate typedExpression
  Except.liftIO $ PrettySimple.pPrint value

type ExceptIO e = ExceptT e IO

parseFromFile :: Parser PhallExpression -> String -> ExceptIO ParserError PhallExpression
parseFromFile parser file = do
  text <- Except.liftIO $ TextIO.readFile file
  Except.liftEither $ Megaparsec.parse parser file text
