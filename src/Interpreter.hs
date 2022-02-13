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
import Text.JSON as Json
import Text.Megaparsec as Megaparsec
import Text.Pretty.Simple as PrettySimple
import TypeEvaluator.TypeEvaluator as TypeEvaluator

runInterpreter :: IO ()
runInterpreter = do
  result <- Except.runExceptT interpret
  case result of
    Left phallError -> print $ Error.message phallError
    Right _ -> return ()

interpret :: ResultIO ()
interpret = do
  inputArguments <- Except.lift System.getArgs
  let sourceFile = List.head inputArguments
  expression <- parseFromFile Parser.parse sourceFile
  Except.liftIO $ PrettySimple.pPrint expression
  expressionType <- Error.toResultIO $ TypeEvaluator.evaluate expression
  Except.liftIO . TextIO.putStrLn $ Type.getTypeName expressionType
  value <- Error.toResultIO $ Evaluator.evaluate expression
  Except.liftIO $ PrettySimple.pPrint value
  Except.liftIO $ PrettySimple.pPrintString $ Json.encode value

parseFromFile :: Parser PhallExpression -> String -> ResultIO PhallExpression
parseFromFile parser file = do
  text <- Except.liftIO $ TextIO.readFile file
  expression <- Monad.lift $ Megaparsec.runParserT parser file text
  Except.withExceptT ParserError $ Except.liftEither expression
