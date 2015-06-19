-- SHOULD THE COMPUTE WEIGHTS USE `NUB`?

module App.Yente.TokenWeightMap 
    ( TokenWeightMap(..)
    , computeWeights
    , scoreTokenList
    , findWeight
    ) where 

{-import App.Yente.Name-}
import Data.List
import qualified Data.Map.Strict as DM


data TokenWeightMap = TokenWeightMap 
  { tokenWeights     :: DM.Map String Double
  , rarestTokenValue :: !Double 
  } deriving (Show, Eq)

-- generate term frequency score map
computeWeights :: [[String]]      -- ^ List of token lists
               -> TokenWeightMap
computeWeights tknsIn =
    TokenWeightMap{ tokenWeights     = tw
                  , rarestTokenValue = rtv
                  }
    where
  score wds = (head wds, log(corpsize/(fromIntegral . length $ wds)))
  tkns      = group . sort . concatMap (nub) $ tknsIn 
  corpsize  = fromIntegral . length $ tknsIn
  tw        = DM.fromList $ map score tkns
  rtv       = maximum . DM.elems $ tw --- This value is stored with the token weight map for efficiency


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




