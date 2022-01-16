module Common (Parser, ParsecError, ParsecStream) where

import Data.Text.Lazy (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser a = Parsec ParsecError ParsecStream a

type ParsecError = Void

type ParsecStream = Text
