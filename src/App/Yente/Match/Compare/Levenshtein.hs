module App.Yente.Match.Compare.Levenshtein
    ( levenshtein
    ) where

import           App.Yente.Core

levenshtein ∷ Name TokenList
            → Name TokenList
            → NameComparison
levenshtein nameA nameB = NameComparison
    { fromName = toNameRaw nameA
    , toName   = toNameRaw nameB
    , score    = 1 - rawDlScore / lengthA
    }
  where
    rawDlScore = fromIntegral $
        restrictedDamerauLevenshteinDistanceText
          defaultEditCosts joinedTokensA joinedTokensB

    lengthA = fromIntegral $ length joinedTokensA

    joinedTokensA, joinedTokensB ∷ Text
    joinedTokensA = unwords . nameData $ nameA
    joinedTokensB = unwords . nameData $ nameB

