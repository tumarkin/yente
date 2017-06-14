module App.Yente.Prelude
  ( module X
  , downcaseString

   -- * Data.Text
  , Text
  , toLower
  , tlength
  , tunwords

  -- * Data.Map.Strict
  , Map
  , elems
  , emptyMap
  , findWithDefault
  , foldrWithKey
  , fromList
  , insertWith
  , intersectionWith
  , keys
  , mapWithKey
  , toList
  , update

  -- * Data.Funcvtion
  , on

  -- * Data.Maybe
  , isJust

  -- * Data.Vector
  , Vector

  -- * EditCosts
  , restrictedDamerauLevenshteinDistanceText

  -- * Unicode
  , unidecode
  ) where

import           Prelude             as X
-- import           ClassyPrelude       as X

import           Control.Applicative as X
import           Control.Arrow       as X
import           Control.Monad       as X
import           Data.Function       (on)
import           Data.List           as X
import           Data.Map.Strict     (Map, elems, empty, findWithDefault,
                                      foldrWithKey, fromList, insertWith,
                                      intersectionWith, keys, mapWithKey, update, toList)
import           Data.Maybe          (isJust)
import           Data.Text           (Text, pack, toLower, unpack)
import           Data.Vector         (Vector)
import           Text.EditDistance   as X

import qualified Data.Text           as T
import qualified Text.Unidecode      as U

downcaseString :: String -> String
downcaseString = unpack . toLower . pack

restrictedDamerauLevenshteinDistanceText :: EditCosts -> Text -> Text -> Int
restrictedDamerauLevenshteinDistanceText ec t1 t2 =
  restrictedDamerauLevenshteinDistance ec (unpack t1) (unpack t2)

tlength :: Text -> Int
tlength = T.length

tunwords :: [Text] -> Text
tunwords = T.unwords

emptyMap = Data.Map.Strict.empty


unidecode :: Text -> Text
unidecode = pack . foldMap U.unidecode . unpack
