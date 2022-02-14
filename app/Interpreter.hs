{-# LANGUAGE OverloadedStrings #-}

module Interpreter (handleCommand) where

import Control.Monad.Except as Except
import Control.Monad.Trans as Monad
import Data.Text.Lazy as Text
import Data.Text.Lazy.IO as TextIO
import Error
import Evaluator.PhallValue as Value
import Evaluator.ValueEvaluator as Evaluator
import Parser.PhallExpression
import Parser.PhallParser as Parser
import Parser.PhallType as Type
import Text.JSON as Json
import Text.Megaparsec as Megaparsec
import Text.Pretty.Simple as PrettySimple
import TypeEvaluator.TypeEvaluator as TypeEvaluator

handleCommand :: [String] -> IO ()
handleCommand ["parse", file] = runResult $ do
  expression <- parseFile file
  prettyPrintColor $ show expression
handleCommand ["eval-type", file] = runResult $ do
  expression <- parseFile file
  expressionType <- Error.toResultIO $ TypeEvaluator.evaluate expression
  prettyPrintColor $ Text.unpack $ Type.getTypeName expressionType
handleCommand ["eval", file] = runResult $ do
  value <- evaluateFile file
  prettyPrintColor $ show value
handleCommand ["eval-json", file] = runResult $ do
  value <- evaluateFile file
  prettyPrintColor $ Json.encode value
handleCommand ["eval-json-raw", file] = runResult $ do
  value <- evaluateFile file
  prettyPrintNoColor $ Json.encode value
handleCommand ["check-type", file] = runResult $ do
  expression <- parseFile file
  _ <- Error.toResultIO $ TypeEvaluator.evaluate expression
  prettyPrintColor ("File '" <> file <> "' types correctly.")
handleCommand _ =
  TextIO.putStrLn
    "Unknown command. Try `parse|eval-type|eval|eval-json|eval-json-raw|check-type <file.phall>`"

runResult :: ResultIO () -> IO ()
runResult resultIO = do
  result <- Except.runExceptT resultIO
  case result of
    Left phallError -> TextIO.putStrLn $ Error.message phallError
    Right _ -> return ()

parseFile :: String -> ResultIO PhallExpression
parseFile file = do
  text <- Except.liftIO $ TextIO.readFile file
  expression <- Monad.lift $ Megaparsec.runParserT Parser.parse file text
  Except.withExceptT ParserError $ Except.liftEither expression

evaluateFile :: String -> ResultIO PhallValue
evaluateFile file = do
  expression <- parseFile file
  _ <- Error.toResultIO $ TypeEvaluator.evaluate expression
  Error.toResultIO $ Evaluator.evaluate expression

prettyPrintColor :: String -> ResultIO ()
prettyPrintColor = do
  let smallerIndentOptions = defaultOutputOptionsDarkBg {outputOptionsIndentAmount = 2}
  Except.liftIO . PrettySimple.pPrintStringOpt CheckColorTty smallerIndentOptions

prettyPrintNoColor :: String -> ResultIO ()
prettyPrintNoColor = do
  let smallerIndentOptions = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 2}
  Except.liftIO . PrettySimple.pPrintStringOpt NoCheckColorTty smallerIndentOptions
