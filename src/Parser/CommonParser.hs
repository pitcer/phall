module Parser.CommonParser where

import Common
import Control.Applicative
import Lexer.PhallLexer as Lexer
import Lexer.Symbol as Symbol
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
