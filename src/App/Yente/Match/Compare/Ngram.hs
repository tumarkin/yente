module App.Yente.Match.Compare.Ngram
  ( nGramDistance
  ) where

import qualified Data.Map.Strict as M
import           Data.Text       (tails)

import           App.Yente.Core

nGramDistance ∷ Int
              → Text
              → Text
              → Double
nGramDistance n textA textB =
    if   null agrams || null bgrams then 0
    else count common/sqrt(count agrams * count bgrams)
  where
    common = intersectionWith min agrams bgrams
    agrams = formNgrams n textA
    bgrams = formNgrams n textB

    count ∷ M.Map Text Int → Double
    count = fromIntegral . M.foldr' (+) 0

formNgrams ∷ Int → Text → Map Text Int
formNgrams n t
    -- A word shorter than or equal to the ngram length is taken as is
    | n >= length t = singletonMap t 1 
    | otherwise     = unionsWith (+) . map (`singletonMap` 1) $ ngrams
  where
    ngrams = filter ((==) n . length) . map (take n) . tails $ t
