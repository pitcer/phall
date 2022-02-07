{-# LANGUAGE NamedFieldPuns #-}

module Environment where

import Data.Map as Map
import FullSet
import Parser.PhallType

newtype Environment a = Environment
  { entries :: Map Name a
  }
  deriving (Show)

empty :: Environment a
empty = Environment {entries = Map.empty}

lookup :: Name -> Environment a -> Maybe a
lookup name Environment {entries} =
  Map.lookup name entries

with :: Name -> a -> Environment a -> Environment a
with name value Environment {entries} = do
  let modifiedEntries = Map.insert name value entries
  Environment {entries = modifiedEntries}

union :: Environment a -> Environment a -> Environment a
union
  Environment {entries = firstEntries}
  Environment {entries = secondEntries} = do
    let unionEntries = Map.union firstEntries secondEntries
    Environment {entries = unionEntries}

restrict :: Environment a -> FullSet Name -> Environment a
restrict environment Full = environment
restrict Environment {entries} (NotFull keySey) = do
  let restrictedEntries = Map.restrictKeys entries keySey
  Environment {entries = restrictedEntries}
