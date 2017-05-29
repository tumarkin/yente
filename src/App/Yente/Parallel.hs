module App.Yente.Parallel
    ( pMap
    , pMapChunk
    ) where

import Control.Parallel
import Control.Parallel.Strategies
import App.Yente.Prelude

pMap :: (NFData b)
     => (a-> b)
     -> [a] -> [b]
pMap f as =
  let xs = map f as
      bs = xs `using` parList rdeepseq
  in bs


pMapChunk :: (NFData b)
          => Int
          -> (a -> b)
          -> [a]
          -> [b]
pMapChunk chunkSize f as =
  let xs = map f as
      bs = xs `using` parListChunk chunkSize rdeepseq
  in bs



