module Interpreter (interpret) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO (readFile)
import qualified PhallParser as Parser (parse)
import qualified Text.Megaparsec as Megaparsec

interpret :: IO ()
interpret = do
  result <- parseFromFile (Megaparsec.many Parser.parse <* Megaparsec.eof) "test.phall"
  case result of
    Left errorBundle -> putStrLn $ Megaparsec.errorBundlePretty errorBundle
    Right tokens -> print tokens

parseFromFile ::
  Megaparsec.Parsec e Text a -> String -> IO (Either (Megaparsec.ParseErrorBundle Text e) a)
parseFromFile parser file = Megaparsec.parse parser file <$> TextIO.readFile file
