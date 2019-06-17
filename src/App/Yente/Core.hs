module App.Yente.Core
  (
  -- Reexported modules
    module X
  , module App.Yente.Core.Concurrent
  , module App.Yente.Core.Types
  , module App.Yente.Core.IO

  -- * Data.Map.Strict
  , module DMS
  , emptyMap

  -- * EditCosts
  , restrictedDamerauLevenshteinDistanceText

  -- * String conversion
  , unidecode
  , cs
  ) where

-- import ClassyPrelude hiding (group)

import           ClassyPrelude             as X hiding (Map, elems,
                                                 findWithDefault, foldrWithKey,
                                                 fromList, group, insertWith,
                                                 intersectionWith, keys,
                                                 mapWithKey, toList, update)
import           Data.Map.Strict           as DMS (Map, elems, findWithDefault,
                                                   foldrWithKey, fromList,
                                                   insertWith, intersectionWith,
                                                   keys, mapWithKey, toList,
                                                   update)
import qualified Data.Map.Strict
import           Data.String.Conversions   (cs)
import           Text.EditDistance         as X
import qualified Text.Unidecode            as U

import           App.Yente.Core.Concurrent
import           App.Yente.Core.Types
import           App.Yente.Core.IO

restrictedDamerauLevenshteinDistanceText ∷ EditCosts → Text → Text → Int
restrictedDamerauLevenshteinDistanceText ec t1 t2 =
  restrictedDamerauLevenshteinDistance ec (cs t1) (cs t2)

emptyMap = Data.Map.Strict.empty

unidecode ∷ Text → Text
unidecode = pack . foldMap U.unidecode . unpack

