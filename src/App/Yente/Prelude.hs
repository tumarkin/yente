module App.Yente.Prelude
  ( module X

  -- * Data.Map.Strict
  , module DMS
  , emptyMap

  -- * EditCosts
  , restrictedDamerauLevenshteinDistanceText

  -- * String conversion
  , unidecode
  , cs
  ) where

-- Replaces ClassyPrelude lazy map with strict

import           ClassyPrelude           as X hiding (Map, elems,
                                               findWithDefault, foldrWithKey,
                                               fromList, insertWith,
                                               intersectionWith, keys,
                                               mapWithKey, toList, update)
import           Data.Map.Strict         as DMS (Map, elems, findWithDefault,
                                                 foldrWithKey, fromList,
                                                 insertWith, intersectionWith,
                                                 keys, mapWithKey, toList,
                                                 update)
import qualified Data.Map.Strict
import           Data.String.Conversions (cs)
import           Text.EditDistance       as X
import qualified Text.Unidecode          as U

restrictedDamerauLevenshteinDistanceText ∷ EditCosts → Text → Text → Int
restrictedDamerauLevenshteinDistanceText ec t1 t2 =
  restrictedDamerauLevenshteinDistance ec (unpack t1) (unpack t2)

emptyMap = Data.Map.Strict.empty


unidecode ∷ Text → Text
unidecode = pack . foldMap U.unidecode . unpack

