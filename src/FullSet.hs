module FullSet where

import Data.Set as Set

data FullSet a
  = Full
  | NotFull (Set a)
  deriving (Show, Eq)
