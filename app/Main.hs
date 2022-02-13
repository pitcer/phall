module Main where

import Interpreter
import System.Environment as System

main :: IO ()
main = do
  inputArguments <- System.getArgs
  Interpreter.handleCommand inputArguments
