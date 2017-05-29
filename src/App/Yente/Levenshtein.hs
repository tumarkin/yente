module App.Yente.Levenshtein 
    ( levenshtein
    ) where 

import App.Yente.Types
import App.Yente.Prelude

import qualified Data.Text as T


levenshtein :: Name -> Name -> NameComparison
levenshtein nameA nameB = NameComparison 
  { fromComparison = nameA
  , toComparison   = nameB
  , score          = 1 - fromIntegral (restrictedDamerauLevenshteinDistanceText defaultEditCosts joinedTokensA joinedTokensB) / fromIntegral (tlength joinedTokensA)
  }
  where
    joinedTokensA = T.unwords . tokens $ nameA
    joinedTokensB = T.unwords . tokens $ nameB




