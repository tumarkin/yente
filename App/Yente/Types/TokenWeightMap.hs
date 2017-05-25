module App.Yente.Types.TokenWeightMap
    ( TokenWeightMap(..)
    , scoreTokenList
    , findWeight
    ) where


import App.Yente.Prelude

data TokenWeightMap = TokenWeightMap
  { tokenWeights     :: Map Text Double
  , rarestTokenValue :: !Double
  } deriving (Show, Eq)

-- score a list of words with tf
scoreTokenList :: TokenWeightMap -> [Text] -> Double
scoreTokenList _   []       = 0
scoreTokenList twm wordlist = sum [(twm `findWeight` x )**2 | x <- wordlist ]

-- Find the weight for a token, returning rarest token value if token is not weighted
findWeight :: TokenWeightMap -> Text -> Double
findWeight twm s =  findWithDefault rtv s tf
    where
  tf   = tokenWeights twm
  rtv  = rarestTokenValue twm




