module App.Yente.Parallel 
    ( pMap
    , pMapChunk
    ) where 

import Control.Parallel
import Control.Parallel.Strategies

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
    




{-fib 1 = 1-}
{-fib 2 = 1-}
{-fib n = fib(n-1) + fib(n-2)-}



{-pFib :: [Int] -> [Int]-}
{-pFib = pMap fib-}

{-pFibChunk  :: Int -> [Int] -> [Int]-}
{-pFibChunk chunkSize = pMapChunk chunkSize fib-}




{-main = do-}
{-  let l = [26..35]-}
{-      z = pFib l-}
{-  putStrLn (show z)-}

