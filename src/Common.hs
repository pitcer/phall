module Common (Parser, Error (..), EvaluatorError (..), ParserError) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)

type Parser a = Parsec ParsecError ParsecStream a

data Error
  = EvaluatorError EvaluatorError
  | ParserError ParserError
  deriving (Show)

data EvaluatorError
  = InvalidTypeError String
  deriving (Show)

type ParserError = ParseErrorBundle ParsecStream ParsecError

type ParsecError = Void

type ParsecStream = Text
