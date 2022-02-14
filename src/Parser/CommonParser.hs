module Parser.CommonParser where

import Common
import Control.Applicative
import Control.Monad as Monad
import Control.Monad.Trans as Monad
import Data.Either as Either
import Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.IO as TextIO
import FullSet
import Lexer.PhallLexer as Lexer
import Lexer.Symbol as Symbol
import Parser.PhallExpression as Expression
import Parser.PhallType as Type
import qualified Text.Megaparsec as Megaparsec

betweenParenthesisOrNot :: Parser a -> Parser a -> Parser a
betweenParenthesisOrNot freestandingParser betweenParser =
  Megaparsec.try freestandingParser <|> Lexer.betweenParenthesis betweenParser

parseOptionalComma :: Parser a -> Parser a -> Parser a
parseOptionalComma withoutComaParser withComaParser =
  Megaparsec.choice . map Megaparsec.try $
    [ withComaParser <* Lexer.tokenizeSymbol CommaSymbol,
      withoutComaParser
    ]
