module Lib
  ( someFunc,
  )
where

import Text.Megaparsec ()

someFunc :: IO ()
someFunc = do
  putChar 'a'
  c <- getChar
  putStrLn "someFunc"
