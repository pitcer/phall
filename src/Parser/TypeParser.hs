{-# LANGUAGE NamedFieldPuns #-}

module Parser.TypeParser where

import Common
import qualified Data.List as List
import Lexer.PhallLexer as Lexer
import Lexer.Symbol as Symbol
import Parser.CommonParser as Parser
import Parser.PhallType as Type
import qualified Text.Megaparsec as Megaparsec

parseType :: Parser PhallType
parseType = do
  Parser.betweenParenthesisOrNot innerParser innerParser
  where
    innerParser = parseOptionType $ Megaparsec.choice $ complexTypes ++ simpleTypes

parseInnerType :: Parser PhallType
parseInnerType = do
  parseOptionType $
    Parser.betweenParenthesisOrNot
      (Megaparsec.choice simpleTypes)
      (Megaparsec.choice complexTypes)

parseOptionType :: Parser PhallType -> Parser PhallType
parseOptionType innerParser = do
  parsedType <- innerParser
  isOptional <- Megaparsec.optional $ Lexer.tokenizeSymbol QuestionMarkSymbol
  case isOptional of
    Nothing -> return parsedType
    Just _ -> return $ OptionType parsedType

simpleTypes :: [Parser PhallType]
simpleTypes =
  [parseTupleType, parseListType] ++ parseTypeKeywords ++ [parseNamedType]
  where
    parseTupleType = Megaparsec.try $ do
      Lexer.tokenizeSymbol LeftCurlyBracket
      tupleType <- Megaparsec.some $ Parser.parseOptionalComma parseInnerType parseType
      Lexer.tokenizeSymbol RightCurlyBracket
      return $ TupleType tupleType

    parseListType = Megaparsec.try $ do
      Lexer.tokenizeSymbol LeftSquareBracket
      listType <- parseType
      Lexer.tokenizeSymbol RightSquareBracket
      return $ ListType listType

    parseTypeKeywords =
      List.map (Megaparsec.try . fmap Type.fromTypeKeyword . Lexer.tokenizeTypeKeyword) enumValues

    parseNamedType =
      Megaparsec.try $ NamedType <$> Lexer.tokenizeIdentifier

complexTypes :: [Parser PhallType]
complexTypes =
  [parseLambdaType]
  where
    parseLambdaType = Megaparsec.try $ do
      parameterType <- parseInnerType
      Lexer.tokenizeSymbol RightArrowSymbol
      bodyType <- parseType
      return $ LambdaType {Type.parameterType, Type.bodyType}
