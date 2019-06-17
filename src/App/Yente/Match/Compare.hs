module App.Yente.Match.Compare
  ( MatchConfig(..)
  , MisspellingMethod(..)
  , cosine
  , cosineWithMispellings
  ) where


import           App.Yente.Match.Compare.Levenshtein
import           App.Yente.Match.Compare.Ngram
import           App.Yente.Core


--------------------------------------------------------------------------------
-- Match Configuration                                                        --
--------------------------------------------------------------------------------
data MatchConfig = MatchConfig
  { misspellingMethod ∷ Maybe MisspellingMethod
  , subgroupSearch    ∷ Bool
  } deriving (Show)

data MisspellingMethod
  = Levenshtein Double
  | Ngram       Int
  deriving (Show)



--------------------------------------------------------------------------------
-- Cosine similarity functions                                                --
--------------------------------------------------------------------------------

-- | Cosine similarity without any allowance for misspellings.
cosine ∷ Name NormWeights → Name NormWeights → NameComparison
cosine nameA nameB = NameComparison
    { fromName = toNameRaw nameA
    , toName   = toNameRaw nameB
    , score    = sumSquareWeightMap i/(norm nameA * norm nameB)
    }
  where
    i ∷ Map Text CountWeights
    i = intersectionWith (\(ia, w, wsq) (ib, _, _) -> (min ia ib, w, wsq))
        (countWeights nameA) (countWeights nameB)


-- | A sequential, non-exhaustive matcher that allows for misspellings. It finds
-- the highest scoring token pair and removes this pair from further
-- consideration before finding the next highest scoring token pair.
cosineWithMispellings ∷ MisspellingMethod
                      → Name NormWeights
                      → Name NormWeights
                      → NameComparison
cosineWithMispellings mm nameA nameB = NameComparison
    { fromName = toNameRaw nameA
    , toName   = toNameRaw nameB
    , score    = numerator/(norm nameA * norm nameB)
    }
  where
    numerator     = sum (sequentialNumerator nameA nameB scoredCombins)
    scoredCombins = map (scoreTokensWithMispellings mm) $ keyWeightCombinations nameA nameB


sequentialNumerator ∷ Name NormWeights
                    → Name NormWeights
                    → [((Text, Text), Double)]
                    → [Double]
sequentialNumerator nameA nameB scores
  | nullName nameA = []
  | nullName nameB = []
  | otherwise      = maxScore : sequentialNumerator nameA' nameB' scores'

  where

    -- Find the best option
    -- de@((tokenA, tokenB), maxScore) = minimumBy (compare `on` (Down . snd)) scores
    ((tokenA, tokenB), maxScore) = maximumBy (compare `on` snd) nnScores

    -- Update the names
    nameA'   = remove1TokenCount tokenA nameA
    nameB'   = remove1TokenCount tokenB nameB

    nnScores ∷ NonNull [((Text,Text), Double)]
    nnScores = fromMaybe (error "No scores found") $ fromNullable scores

    -- Filter out the scores that are no longer valid
    scores'  = filter (\(k,_) -> k `elem` remainingTokens) scores
    remainingTokens = tokenCombinations nameA' nameB'

    nullName ∷ Name NormWeights → Bool
    nullName = null . countWeights



scoreTokensWithMispellings
  ∷ MisspellingMethod
  → ((Text, Double), (Text, Double)) -- ^ ((Token, Weight), (Token, Weight))
  → ((Text, Text), Double)           -- ^ ((Token, Token), Score)

-- Levenshtein
scoreTokensWithMispellings (Levenshtein pf) ((tokenA,wtA),(tokenB, wtB))
    = ((tokenA, tokenB), (matchFraction ** pf) * (wtA * wtB))
  where
    matchFraction = 1- fromIntegral (restrictedDamerauLevenshteinDistanceText defaultEditCosts tokenA tokenB) / fromIntegral ( max (length tokenA) (length tokenB))

-- Ngram
scoreTokensWithMispellings (Ngram i) ((tokenA,wtA),(tokenB, wtB))
    = ((tokenA, tokenB), nGramDistance i tokenA tokenB * (wtA * wtB))


--------------------------------------------------------------------------------
-- Utility functions                                                          --
--------------------------------------------------------------------------------

-- | Mix all keys
tokenCombinations ∷ Name NormWeights → Name NormWeights → [(Text, Text)]
tokenCombinations a b =
    map tuplify . sequence $ keys . countWeights <$> [a, b]
  where
    tuplify (x:y:_) = (x,y)

-- | Max all keys and weights
keyWeightCombinations ∷ Name NormWeights → Name NormWeights → [((Text, Double), (Text, Double))]
keyWeightCombinations a b = map tuplify . sequence $
    toList . mapWithKey (\k (_, w, _) -> w) . countWeights <$> [a, b]
  where tuplify (x:y:_) = (x,y)


-- | Remove a single token occurance. This would be nicer with lenses.
remove1TokenCount ∷ Text → Name NormWeights → Name NormWeights
remove1TokenCount t n = NormWeights _norm cw' <$ n
  where
    NormWeights{..} = nameData n
    cw' = update (\(c, w, w2) → if c > 1 then Just (c-1, w, w2) else Nothing) t _countWeights


countWeights ∷ Name NormWeights → Map Text CountWeights
countWeights = _countWeights . nameData

norm ∷ Name NormWeights → Double
norm = _norm . nameData
