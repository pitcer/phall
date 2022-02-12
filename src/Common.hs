module Common where

import Data.Text.Lazy as Text
import Data.Void as Void
import Text.Megaparsec as Megaparsec

type ParserT m a = ParsecT ParsecError ParsecStream m a

type Parser a = ParserT IO a

type ParsecError = Void

type ParsecStream = Text

type Name = Text
