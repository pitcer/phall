module Interpreter (runInterpreter) where

import Common
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
  ( liftEither,
    liftIO,
    runExcept,
    runExceptT,
    withExceptT,
  )
import qualified Data.Text.Lazy.IO as TextIO (readFile)
import Error
import qualified PhallEvaluator as Evaluator (evaluate)
import PhallParser (PhallExpression)
import qualified PhallParser as Parser
import qualified Text.Megaparsec as Megaparsec (errorBundlePretty, parse)

type ExceptIO e a = ExceptT e IO a

runInterpreter :: IO ()
runInterpreter = do
  result <- Except.runExceptT interpret
  case result of
    Left (ParserError errorBundle) -> putStrLn $ Megaparsec.errorBundlePretty errorBundle
    Left (EvaluatorError evaluatorError) -> print evaluatorError
    Right _ -> return ()

interpret :: ExceptIO Error ()
interpret = do
  expression <- Except.withExceptT ParserError $ parseFromFile Parser.parse "playground/test.phall"
  Except.liftIO $ print expression
  value <-
    Except.liftEither
      . Except.runExcept
      . Except.withExceptT EvaluatorError
      $ Evaluator.evaluate expression
  Except.liftIO $ print value

parseFromFile ::
  Parser PhallExpression -> String -> ExceptIO ParserError PhallExpression
parseFromFile parser file = do
  text <- Except.liftIO $ TextIO.readFile file
  Except.liftEither $ Megaparsec.parse parser file text
