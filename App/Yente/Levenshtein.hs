module App.Yente.Levenshtein 
    ( levenshtein
    ) where 

import Text.EditDistance

import App.Yente.Types


levenshtein :: Name -> Name -> NameComparison
levenshtein nameA nameB = NameComparison 
  { fromComparison = nameA
  , toComparison   = nameB
  , score          = 1 - fromIntegral (restrictedDamerauLevenshteinDistance defaultEditCosts joinedTokensA joinedTokensB) / fromIntegral (length joinedTokensA)
  }
  where
    joinedTokensA = unwords . tokens $ nameA
    joinedTokensB = unwords . tokens $ nameB




