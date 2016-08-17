module App.Yente.Cosine 
    ( cosine
    , cosineWithMispellings
    , scoreTokensWithMispellings
    ) where 

import Data.List
import qualified Data.Map.Strict as DM
import Data.Maybe (fromJust)
import Control.Applicative
import Text.EditDistance

import App.Yente.Types
-- Name
-- import App.Yente.NameComparison
-- import App.Yente.TokenWeightMap


-- compute cosine
cosine :: TokenWeightMap -> Name -> Name -> NameComparison
cosine wts nameA (nameB@Name{ norm = Nothing }) = cosine wts nameA (normName wts nameB)
cosine wts (nameA@Name{norm = Nothing}) nameB   = cosine wts (normName wts nameA) nameB 
cosine wts nameA nameB  --- The wts can be left blank eventually here 
    = NameComparison { fromComparison = nameA
                     , toComparison   = nameB
                     , score          = score
                     }
  where 
    score        = if Data.List.null intersection then 0
                   else scoreTokenList wts intersection/(fromJust $ (*) <$> norm nameA <*> norm nameB ) 
    intersection = intersect (tokens nameA) (tokens nameB)

    {-score        = if DM.null sharedTokens then 0-}
    {-               else newscore/(fromJust $ (*) <$> norm nameA <*> norm nameB ) -}
    {-sharedTokens = fromJust $ DM.intersection <$> tokenWts nameA <*> tokenWts nameB-}
    {-newscore     = foldl1 (+) . map (**2) . DM.elems $ sharedTokens-}


cosineWithMispellings :: Double          -- Penalty factor
                      -> TokenWeightMap 
                      -> Name 
                      -> Name 
                      -> NameComparison
cosineWithMispellings pf wts nameA (nameB@Name{ norm = Nothing }) = cosineWithMispellings pf wts nameA (normName wts nameB)
cosineWithMispellings pf wts (nameA@Name{norm = Nothing}) nameB   = cosineWithMispellings pf wts (normName wts nameA) nameB 
cosineWithMispellings pf wts nameA nameB   
    = NameComparison { fromComparison = nameA
                     , toComparison   = nameB
                     , score          = numerator/(fromJust $ (*) <$> norm nameA <*> norm nameB ) 
                     }
  where 
    numerator    = maximum (map scoreMix $ tokenWtMixes) -- scoreMix $ head tokenWtMixes 
    scoreMix     = sum . map (scoreTokensWithMispellings pf) 
    tokenWtsA    = fromJust $ tokenWts nameA
    tokenWtsB    = fromJust $ tokenWts nameB
    tokenWtMixes = nub . map (zip tokenWtsA) $ Data.List.permutations tokenWtsB --- all unique variations of token matches





scoreTokensWithMispellings :: Double                 -- ^ penalty factor
                           ->   ( (String, Double),  -- ^ ((Token, Value), (Token, Value))
                                (String, Double) )
                           -> Double                 -- ^ Score
scoreTokensWithMispellings pf ((tokenA,wtA),(tokenB, wtB))
    = (matchFraction ** pf) * (wtA * wtB)
  where 
    matchFraction = 1- fromIntegral (restrictedDamerauLevenshteinDistance defaultEditCosts tokenA tokenB)
                       / fromIntegral ( max (length tokenA) (length tokenB))
