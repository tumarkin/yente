{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module App.Yente.Match.Output
  ( OutputConfig(..)
  , selectResults
  ) where

import App.Yente.Core


data OutputConfig = OutputConfig
  { numberOfResults   ∷ !Int
  , includeTies       ∷ !Bool
  , minimumMatchScore ∷ !Double
  } deriving (Show)

selectResults ∷ OutputConfig 
              → Vector NameComparison 
              → Vector NameComparison
selectResults OutputConfig{..} =
    selectBest includeTies numberOfResults 
      . filterComparison minimumMatchScore


--------------------------------------------------------------------------------
-- Utility functions                                                          --
--------------------------------------------------------------------------------
filterComparison ∷ (IsSequence seq, Element seq ~ NameComparison)
                 ⇒ Double → seq → seq
filterComparison minScore
    = filter (\nc → score nc >= minScore)

-- Select best possibly including ties
selectBest ∷ Bool 
           → Int 
           → Vector NameComparison 
           → Vector NameComparison
selectBest _     _               (null → True) = empty
selectBest False matchesToOutput ncs           = take matchesToOutput . sortBy (flip compare) $ ncs
selectBest True  matchesToOutput ncs           = takeWhile (>= cutoff) sorted
    where
  cutoff ∷ NameComparison
  cutoff = fromMaybe (error "No cutoff") $ (last <$> fromNullable (take matchesToOutput sorted))
  sorted = sortBy (flip compare) ncs

