module App.Yente.Prelude
  ( module X
  , downcaseString

   -- * Data.Text
  , Text
  , toLower
  , tlength

  -- * Data.Map.Strict
  , Map
  , findWithDefault
  , elems
  , fromList
  , insertWith
  , emptyMap

  -- * Data.Funcvtion
  , on

  -- * Data.Maybe
  , isJust

  -- * Data.Vector
  , Vector

  -- * EditCosts
  , restrictedDamerauLevenshteinDistanceText 
  ) where

import           Prelude             as X
-- import           ClassyPrelude       as X

import           Text.EditDistance   as X
import           Data.Maybe                      (isJust)
import           Control.Applicative as X
import           Control.Monad       as X
import           Data.List           as X
import           Data.Function       (on)
import           Data.Map.Strict     (Map, elems, findWithDefault, fromList, insertWith, empty)
import           Data.Text           (Text, toLower, unpack, pack)
import           Data.Vector         (Vector)

import qualified Data.Text           as T

downcaseString :: String -> String
downcaseString = unpack . toLower . pack

restrictedDamerauLevenshteinDistanceText :: EditCosts -> Text -> Text -> Int
restrictedDamerauLevenshteinDistanceText ec t1 t2 =
  restrictedDamerauLevenshteinDistance ec (unpack t1) (unpack t2)

tlength :: Text -> Int
tlength = T.length

emptyMap = Data.Map.Strict.empty
