module Interpreter (runInterpreter) where

import Common
import Control.Monad.Except as Except
import Data.Text.Lazy.IO as TextIO
import Error
import Evaluator.Environment as Environment
import Evaluator.PhallEvaluator as Evaluator
import PhallParser as Parser
import Text.Megaparsec as Megaparsec
import Text.Pretty.Simple as PrettySimple

type ExceptIO e a = ExceptT e IO a

runInterpreter :: IO ()
runInterpreter = do
  result <- Except.runExceptT interpret
  case result of
    Left (ParserError errorBundle) -> Prelude.putStrLn $ Megaparsec.errorBundlePretty errorBundle
    Left (EvaluatorError evaluatorError) -> print $ message evaluatorError
    Right _ -> return ()

interpret :: ExceptIO PhallError ()
interpret = do
  expression <- Except.withExceptT ParserError $ parseFromFile Parser.parse "playground/test.phall"
  Except.liftIO $ PrettySimple.pPrint expression
  value <-
    Except.liftEither
      . Except.runExcept
      . Except.withExceptT EvaluatorError
      $ Evaluator.evaluate Environment.empty expression
  Except.liftIO $ PrettySimple.pPrint value

parseFromFile ::
  Parser PhallExpression -> String -> ExceptIO ParserError PhallExpression
parseFromFile parser file = do
  text <- Except.liftIO $ TextIO.readFile file
  Except.liftEither $ Megaparsec.parse parser file text
