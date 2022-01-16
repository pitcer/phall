module Error
  ( Error (..),
    EvaluatorError (..),
    ParserError,
  )
where

import Common
import Text.Megaparsec (ParseErrorBundle)

data Error
  = EvaluatorError EvaluatorError
  | ParserError ParserError
  deriving (Show)

data EvaluatorError
  = InvalidTypeError String
  deriving (Show)

type ParserError = ParseErrorBundle ParsecStream ParsecError
