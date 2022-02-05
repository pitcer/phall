module Common (Parser, ParsecError, ParsecStream) where

import Data.Text.Lazy as Text
import Data.Void as Void
import Text.Megaparsec as Megaparsec

type Parser a = Parsec ParsecError ParsecStream a

type ParsecError = Void

type ParsecStream = Text
