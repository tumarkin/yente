{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module App.Yente.Core.Types (
  -- * Names
    Name(..)

  -- * Basic types
  , TokenList(..)

  -- * Counter
  , TokenCounter(..)
  , emptyCounter
  , addToken
  , NormWeights(..)
  , CountWeights

  -- ** Conversion
  , toNameRaw

  -- -- ** Functions
  , sumSquareWeightMap

  -- * Name Comparison
  , NameComparison(..)

  -- * Token weight maps
  , TokenWeightMap(..)
  , findWeight

  ) where


import           ClassyPrelude   hiding (group)
import qualified Data.Map.Strict as M


--------------------------------------------------------------------------------
-- Name types                                                                 --
--------------------------------------------------------------------------------

-- Name data type
data Name d = Name
  { idx      ∷ !Text
  , name     ∷ !Text
  , group    ∷ !Text
  , nameData ∷ !d
  } deriving (Show, Eq, Functor)

-- | A counter of words and tokens and the number of occurences in a name.
newtype TokenCounter = TokenCounter {unTokenCounter ∷ M.Map Text Int}
  deriving (Show)

emptyCounter ∷ TokenCounter
emptyCounter = TokenCounter M.empty

addToken ∷ TokenCounter → Text → TokenCounter
addToken (TokenCounter c) t = TokenCounter . insertWith (+) t 1 $ c

-- | Check if two names have the same group
sameGroup ∷ Name a → Name b → Bool
sameGroup a b = group a == group b

data NormWeights = NormWeights
  { _norm         ∷ !Double
  , _countWeights ∷ !(M.Map Text CountWeights)
  } deriving (Eq, Show)

-- | A type alias for the number of tokens occurance in a word,
-- the token weight, and the squared token weight.
type CountWeights = (Int, Double, Double)


-- | A name in which otherData contains a map of each processed token and the
-- token occurance count.
type TokenList = [Text]


-- | A map that knows its highest possible value.
data TokenWeightMap = TokenWeightMap
  { tokenWeights     ∷ !(Map Text Double)
  , rarestTokenValue ∷ !Double
  } deriving (Show, Eq)

-- | Find the weight for a token, returning rarest token value if token is
-- not weighted
findWeight ∷ TokenWeightMap → Text → Double
findWeight TokenWeightMap{..} t =  findWithDefault rarestTokenValue t tokenWeights

-- | Score a list of words with tf
sumSquareWeightMap ∷ Map Text CountWeights → Double -- (Int, Double) → Double
sumSquareWeightMap = M.foldrWithKey squareWeights 0
  where
    squareWeights ∷ Text → CountWeights → Double → Double
    squareWeights _ (i, _, wsq) cum = cum + fromIntegral i * wsq

-- | Name comparisons
data NameComparison = NameComparison
  { fromName ∷ !(Name ())
  , toName   ∷ !(Name ())
  , score    ∷ !Double
  } deriving (Show, Eq)

instance Ord NameComparison where
  compare = compare `on` score

toNameRaw ∷ Name a → Name ()
toNameRaw n = () <$ n



