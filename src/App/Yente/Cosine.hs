module App.Yente.Cosine
    ( cosine
    , cosineWithMispellings
    ) where

import           Data.Ord          (Down (..))

import           App.Yente.Prelude
import           App.Yente.Types

-- import qualified Data.Text         as T
-- import           Debug.Trace



-- | Cosine similarity without any allowance for misspellings.
cosine :: NameNormed -> NameNormed -> NameComparison
cosine nameA nameB
  = NameComparison { fromName = toNameRaw nameA
                   , toName   = toNameRaw nameB
                   , score    = scoreMap i/(norm nameA * norm nameB)
                   }

  where
    i :: Map Text CountWeights
    i = intersectionWith (\(ia, w, wsq) (ib, _, _) -> (min ia ib, w, wsq))
        (countWeights nameA) (countWeights nameB)


-- | A sequential, non-exhaustive matcher that allows for misspellings. It finds
-- the highest scoring token pair and removes this pair from further
-- consideration before finding the next highest scoring token pair.
cosineWithMispellings :: Double          -- ^ Penalty factor
                      -> NameNormed
                      -> NameNormed
                      -> NameComparison
cosineWithMispellings pf nameA nameB =
  NameComparison { fromName = toNameRaw nameA
                 , toName   = toNameRaw nameB
                 , score    = numerator/(norm nameA * norm nameB)
                 }

  where
    numerator    = sum (sequentialNumerator nameA nameB scoredCombins)
    scoredCombins = map (scoreTokensWithMispellings pf) $ keyWeightCombinations nameA nameB


sequentialNumerator :: NameNormed
                    -> NameNormed
                    -> [((Text, Text), Double)]
                    -> [Double]
sequentialNumerator nameA nameB scores
  | nullName nameA = []
  | nullName nameB = []
  | otherwise      = maxScore : sequentialNumerator nameA' nameB' scores'

  where

    -- Find the best option
    de@((tokenA, tokenB), maxScore) = minimumBy (compare `on` (Down . snd)) scores

    -- Update the names
    nameA'   = remove1TokenCount tokenA nameA
    nameB'   = remove1TokenCount tokenB nameB

    -- Filter out the scores that are no longer valid
    scores'  = filter (\(k,_) -> k `elem` remainingTokens) scores
    remainingTokens = tokenCombinations nameA' nameB'

    nullName :: NameNormed -> Bool
    nullName = null . countWeights



scoreTokensWithMispellings
  :: Double                           -- ^ penalty factor
  -> ((Text, Double), (Text, Double)) -- ^ ((Token, Weight), (Token, Weight))
  -> ((Text, Text), Double)           -- ^ ((Token, Token), Score)
scoreTokensWithMispellings pf ((tokenA,wtA),(tokenB, wtB))
    = ((tokenA, tokenB), (matchFraction ** pf) * (wtA * wtB))
  where
    matchFraction = 1- fromIntegral (restrictedDamerauLevenshteinDistanceText defaultEditCosts tokenA tokenB) / fromIntegral ( max (tlength tokenA) (tlength tokenB))


-- Utility functions
--
-- | Mix all keys
tokenCombinations :: NameNormed -> NameNormed -> [(Text, Text)]
tokenCombinations a b = map tuplify . sequence $ keys . countWeights <$> [a, b]
  where tuplify (x:y:_) = (x,y)

-- | Max all keys and weights
keyWeightCombinations :: NameNormed -> NameNormed -> [((Text, Double), (Text, Double))]
keyWeightCombinations a b = map tuplify . sequence $
    toList . mapWithKey (\k (_, w, _) -> w) . countWeights <$> [a, b]
  where tuplify (x:y:_) = (x,y)



-- -- -- Debugging
-- js1 = NameNormed $ Name (T.pack "5167726") (T.pack "Smith, Joe") Nothing NormWeights{_norm=9.594395076687045, _countWeights = Data.Map.fromList [(T.pack "joe", (1, 6.055612366931734, 6.055612366931734**2)), (T.pack "smith", (1 ,7.441906728051625, 7.441906728051625**2))]}

-- js2 = NameNormed $ Name (T.pack "5167726") (T.pack "Smyth, Job") Nothing NormWeights{_norm=9.594395076687045, _countWeights = Data.Map.fromList [(T.pack "job", (1, 6.055612366931734, 6.055612366931734**2)), (T.pack "smyth", (1 ,7.441906728051625, 7.441906728051625**2))]}


