module App.Yente.Types.TokenWeightMap 
    ( TokenWeightMap(..)
    , scoreTokenList
    , findWeight
    ) where 

import Data.List
import qualified Data.Map.Strict as DM


data TokenWeightMap = TokenWeightMap 
  { tokenWeights     :: DM.Map String Double
  , rarestTokenValue :: !Double 
  } deriving (Show, Eq)

-- score a list of words with tf
scoreTokenList :: TokenWeightMap -> [String] -> Double
scoreTokenList _   []       = 0
scoreTokenList twm wordlist = sum [(twm `findWeight` x )**2 | x <- wordlist ]

-- Find the weight for a token, returning rarest token value if token is not weighted
findWeight :: TokenWeightMap -> String -> Double
findWeight twm s =  DM.findWithDefault rtv s tf
    where 
  tf   = tokenWeights twm
  rtv  = rarestTokenValue twm




