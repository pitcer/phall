module Interpreter (runInterpreter) where

import Common
import Control.Monad.Except as Except
import Control.Monad.Trans as Monad
import qualified Data.List as List
import Data.Text.Lazy.IO as TextIO
import Error
import Evaluator.ValueEvaluator as Evaluator
import Parser.PhallExpression
import Parser.PhallParser as Parser
import Parser.PhallType as Type
import System.Environment as System
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
  inputArguments <- Except.lift System.getArgs
  let sourceFile = List.head inputArguments
  expression <- Except.withExceptT ParserError $ parseFromFile Parser.parse sourceFile
  Except.liftIO $ PrettySimple.pPrint expression
  (expressionType) <-
    Except.liftEither . Except.runExcept . Except.withExceptT TypeError $
      TypeEvaluator.evaluate expression
  Except.liftIO . TextIO.putStrLn $ Type.getTypeName expressionType
  value <-
    Except.liftEither . Except.runExcept . Except.withExceptT EvaluatorError $
      Evaluator.evaluate expression
  Except.liftIO $ PrettySimple.pPrint value

type ExceptIO e = ExceptT e IO

parseFromFile :: Parser PhallExpression -> String -> ExceptIO ParserError PhallExpression
parseFromFile parser file = do
  text <- Except.liftIO $ TextIO.readFile file
  expression <- Monad.lift $ Megaparsec.runParserT parser file text
  Except.liftEither expression
