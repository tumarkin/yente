module App.Yente.Levenshtein
    ( levenshtein
    ) where

import           App.Yente.Prelude
import           App.Yente.Types


levenshtein ∷ NameTokenList → NameTokenList → NameComparison
levenshtein nameA nameB = NameComparison
  { fromName = toNameRaw nameA
  , toName   = toNameRaw nameB
  , score    = 1 - fromIntegral (restrictedDamerauLevenshteinDistanceText defaultEditCosts joinedTokensA joinedTokensB) / fromIntegral (length joinedTokensA)
  }
  where
    joinedTokensA = unwords . tokenList $ nameA
    joinedTokensB = unwords . tokenList $ nameB





