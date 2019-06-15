module App.Yente.Ngram
  ( nGramDistance
  ) where

import qualified Data.Map          as M
import           Data.Text         (tails)

import           App.Yente.Prelude

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

    count ∷ Map Text Int → Double
    count = fromIntegral . M.foldr' (+) 0


formNgrams ∷ Int → Text → Map Text Int
formNgrams n t = M.unionsWith (+) . map (\ng → singletonMap ng 1) $ ngrams
  where
    ngrams = filter ((==) n . length) . map (take n) . tails $ t
