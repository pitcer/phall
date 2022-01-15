module Common (Parser) where

import Data.Text (Text)
import Data.Void (Void)
import qualified Text.Megaparsec as Megaparsec

type Parser = Megaparsec.Parsec Void Text
