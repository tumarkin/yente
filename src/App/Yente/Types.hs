{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Yente.Types (
  -- * Names

  -- ** Basic types
    Name(..)
  , NameRaw(..)
  , NameTokenList(..)
  , tokenList
  , NameTokenCount(..)
  , tokens
  , NameNormed(..)
  , norm
  , countWeights

  , NormWeights(..)
  , CountWeights

  -- ** Conversion
  , toNameRaw

  -- ** Functions
  , emptyName
  , sameGroup
  -- , encodeName
  , encodeNameTokenList
  , toNameTokenCount
  , normName
  -- , crossProduct
  , sumSquareWeightMap
  , remove1TokenCount


  -- * Name Comparison
  , NameComparison(..)

  -- * Token weight maps
  , TokenWeightMap(..)
  , findWeight
  , computeIDF

  -- * Application modes
  , YenteMode(..)
  , YenteOptions(..)
  , PhoneticAlgorithm(..)

  ) where


import           Control.DeepSeq
import           Data.Char         (isAlphaNum, isLetter)
import           Data.List         (nub)
import qualified Data.Text         as T

import           App.Yente.Prelude hiding (group)
import qualified App.Yente.Prelude


-- Name data type
data Name d = Name
  { idx       :: !Text
  , name      :: !Text
  , group     :: !(Maybe Text)
  , otherData :: !d
  } deriving (Show, Eq)

-- class NameLike n where
--   -- | Check if two names have the same group
sameGroup :: Name a -> Name b -> Bool
sameGroup a b = group a == group b

-- | A name type with no extra data
type NameRaw = Name ()

toNameRaw :: Name a -> NameRaw
toNameRaw n = n{otherData = ()}

-- | A name containing a map of each processed token and the token
-- occurance count in otherData.
type NameTokenList = Name [Text]

tokenList :: NameTokenList -> [Text]
tokenList = otherData

-- | A name containing a map of each processed token and the token
-- occurance count in otherData.
type NameTokenCount = Name (Map Text Int)

tokens :: NameTokenCount -> Map Text Int
tokens = otherData

-- | A name containing a norm and a map from each token to a count,
-- weight, and weight squared in otherData.
type NameNormed = Name NormWeights

data NormWeights = NormWeights
  { _norm    :: !Double
  , _countWeights :: !(Map Text CountWeights)
  } deriving (Eq, Show)

-- | A type alias for the number of tokens occurance in a word,
-- the token weight, and the squared token weight.
type CountWeights = (Int, Double, Double)

norm :: NameNormed -> Double
norm = _norm . otherData

countWeights :: NameNormed -> Map Text CountWeights
countWeights = _countWeights . otherData




-- | NFData instances for parallelization.
instance NFData d => NFData (Name d) where
  rnf Name{ idx      = i
          , name     = n
          , group    = g
          , otherData = d
          } =
    i `seq` n `seq` g `seq` d `seq` ()

instance NFData NormWeights where
  rnf NormWeights{ _norm    = n
                 , _countWeights = ws
                 } = n `seq` ws `seq` ()

-- | Function to create empty name data from id and name strings
-- during cassava import
emptyName :: Text       -- ^ Id
          -> Text       -- ^ Name
          -> Maybe Text -- ^ Group
          -> NameRaw
emptyName i n g = Name
    { idx        = i
    , name      = n
    , group     = g
    , otherData = ()
    }


-- | Encode the name tokens
encodeName  :: Bool           -- ^ Retain numeric characters
            -> (Text -> Text) -- ^ Encoding function
            -> NameRaw       -- ^ Source name
            -> NameTokenCount
encodeName retainNumeric encode n =
    n{otherData = unCounter . countTokens . tokenList $ ntl}

  where

    ntl = encodeNameTokenList retainNumeric encode n


encodeNameTokenList  :: Bool          -- ^ Retain numeric characters
                     -> (Text -> Text) -- ^ Encoding function
                     -> NameRaw       -- ^ Source name
                     -> NameTokenList
encodeNameTokenList retainNumeric encode n
  = n{otherData = tokenCounts}
  where
    tokenCounts = map (encode . toLower) . T.words . T.map filterLetters $ name n
    filterLetters x = if charFilter x then x else ' '
    charFilter      = if retainNumeric then isAlphaNum else isLetter

toNameTokenCount :: NameTokenList -> NameTokenCount
toNameTokenCount n = n{otherData = unCounter . countTokens . tokenList $ n}



-- | Compute the name norm score given a token weight map
normName :: TokenWeightMap -> NameTokenCount -> NameNormed
normName twm n =
  n{ otherData =
    NormWeights{ _norm = sqrt $ sumSquareWeightMap tw
               , _countWeights = tw
               }
  }
  where
    transformValues :: Text -> Int -> CountWeights -- (Int, Double, Double)
    transformValues k c = (c, w, w**2)
      where w = twm `findWeight` k

    tw :: Map Text CountWeights -- (Int, Double, Double)
    tw = mapWithKey transformValues (tokens n)

-- | Remove a single token occurance. This would be nicer with lenses.
remove1TokenCount :: Text -> NameNormed -> NameNormed
remove1TokenCount t n = n{otherData = NormWeights _norm cw'}
  where
    NormWeights{..} = otherData n
    cw' = update (\(c, w, w2) -> if c > 1 then Just (c-1, w, w2) else Nothing) t _countWeights


-- | Name comparisons
data NameComparison = NameComparison
  { fromName :: !NameRaw
  , toName   :: !NameRaw
  , score    :: !Double
  } deriving (Show, Eq)

instance Ord NameComparison where
  compare = compare `on` score

instance NFData NameComparison where
  rnf NameComparison{
        fromName = fn
      , toName = tn
      , score  = s
      } = rnf fn `seq` rnf tn `seq` s `seq` ()


-- | A map that knows its highest possible value.
data TokenWeightMap = TokenWeightMap
  { tokenWeights     :: !(Map Text Double)
  , rarestTokenValue :: !Double
  } deriving (Show, Eq)


computeIDF :: [NameTokenCount] -- ^ List of names
           -> TokenWeightMap
computeIDF ns =
  TokenWeightMap{ tokenWeights     = tw
                , rarestTokenValue = maximum .  elems $ tw
                }
  where
    tw               = unCounter $ scoreFcn <$> tokenCounts
    corpsize         = fromIntegral $ length ns
    unqiueNameTokens = nub . keys . tokens <$> ns
    tokenCounts      = countTokens $ concat unqiueNameTokens

    scoreFcn :: Int -> Double
    scoreFcn i = log(corpsize/fromIntegral i)


-- -- | Cross product
-- crossProduct :: NameNormed -> NameNormed -> Double
-- crossProduct a b =

-- | Score a list of words with tf
sumSquareWeightMap :: Map Text CountWeights -> Double -- (Int, Double) -> Double
sumSquareWeightMap = foldrWithKey squareWeights 0
  where
    squareWeights :: Text -> CountWeights -> Double -> Double
    squareWeights _ (i, _, wsq) cum = cum + fromIntegral i * wsq


-- | Find the weight for a token, returning rarest token value if token is
-- not weighted
findWeight :: TokenWeightMap -> Text -> Double
findWeight TokenWeightMap{..} t =  findWithDefault rarestTokenValue t tokenWeights




-- | Application modes
data YenteMode = Cosine | Levenshtein

-- | Run-time options
data YenteOptions = YenteOptions
  { fromFile           :: String
  , toFile             :: String
  , phoneticAlgorithm  :: Maybe PhoneticAlgorithm
  , retainNumeric      :: Bool
  , retainUnicode      :: Bool
  , maxTokenLength     :: Maybe Int

  , misspellingPenalty :: Maybe Double
  , subgroupSearch     :: Bool

  , numberOfResults    :: Int
  , includeTies        :: Bool
  , minimumMatchScore  :: Double

  , outputFile         :: Maybe String
  } deriving (Show) --  Data) , Typeable)

data PhoneticAlgorithm 
  = Phonix
  | Soundex
  deriving (Eq, Show)


-- | A counter
newtype Counter i = Counter {unCounter :: Map Text i}
  deriving (Show, Functor)

emptyCounter :: Counter a
emptyCounter = Counter emptyMap

addText :: Counter Int -> Text -> Counter Int
addText c t = Counter . insertWith (+) t 1 $ unCounter c

countTokens :: [Text] -> Counter Int
countTokens = foldl' addText emptyCounter



