{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module App.Yente.Core.Types (
  -- * Names
    Name(..)
  , nameData

  -- * Basic types

  , TokenList(..)


  -- , NameRaw(..)
  -- , NameTokenList(..)
  -- , tokenList
  -- , NameTokenCount(..)
  -- , tokens
  -- , NameNormed(..)
  -- , norm
  -- , countWeights
  --

  -- * Counter
  , TokenCounter(..)
  , emptyCounter
  , addToken
  , NormWeights(..)
  , CountWeights

  -- ** Conversion
  , toNameRaw

  -- -- ** Functions
  -- , sameGroup
  -- , encodeNameTokenList
  -- , toNameTokenCount
  -- , normName
  -- -- , crossProduct
  , sumSquareWeightMap
  -- , remove1TokenCount


  -- * Name Comparison
  , NameComparison(..)

  -- * Token weight maps
  , TokenWeightMap(..)
  , findWeight
  -- , computeIDF

  -- -- * Application modes
  -- , YenteOptions(..)
  -- , PreprocessingOptions(..)
  -- , MatchingOptions(..)
  -- , MisspellingMethod(..)
  -- , OutputOptions(..)
  -- , PhoneticAlgorithm(..)
  -- -- , PenaltyFunctionCoefficient

  ) where


import           ClassyPrelude   hiding (group)
import qualified Data.Map.Strict as M



-- import           Data.Csv          hiding (Name)
-- import           ClassyPrelude           as X hiding (Map, elems,
  --                                              findWithDefault, foldrWithKey,
  --                                              fromList, insertWith,
  --                                              intersectionWith, keys,
  --                                              mapWithKey, toList, update, group)
-- import           Data.Map.Strict         as DMS (Map, elems, findWithDefault,
  --                                                foldrWithKey, fromList,
  --                                                insertWith, intersectionWith,
  --                                                keys, mapWithKey, toList,
  --                                                update)


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













--------------------------------------------------------------------------------
-- To remove                                                                  --
--------------------------------------------------------------------------------

-- -- | A name in which otherData contains a map of each processed
-- -- | A name type with no extra
-- type NameRaw = Name ()

-- -- token and the token occurance count.
-- type TokenCount = M.Map Text Int

-- tokenList ∷ NameTokenList → [Text]
-- tokenList = otherData

-- tokens ∷ NameTokenCount → Map Text Int
-- tokens = otherData

-- -- | A name in which otherData contains a norm and a map from each token to a count,
-- -- weight, and weight squared.
-- type NameNormed = Name NormWeights

  -- , emptyName
-- -- | Function to create empty name data from id and name strings
-- -- during cassava import
-- emptyName ∷ Text -- ^ Id
  --         → Text -- ^ Name
  --         → Text -- ^ Group
  --         → NameRaw
-- emptyName i n g = Name
  --   { idx       = i
  --   , name      = n
  --   , group     = g
  --   , otherData = ()
  --   }

-- -- -- | Cross product
-- -- crossProduct ∷ NameNormed → NameNormed → Double
-- -- crossProduct a b =
