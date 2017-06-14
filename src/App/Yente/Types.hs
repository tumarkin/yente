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
  , NameLike

  -- , NormWeights(..)
  , CountWeights

  -- ** Conversion
  , toNameRaw

  -- ** Functions
  , emptyName
  , sameGroup
  , encodeName
  , encodeNameTokenList  
  , normName
  -- , crossProduct
  , scoreMap
  , remove1TokenCount


  -- * Name Comparison
  , NameComparison(..)

  -- * Token weight maps
  , TokenWeightMap(..)
  , findWeight
  , computeIDF

  -- * Application modes
  , YenteMode(..)

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

class NameLike n where
  -- | Check if two names have the same group
  sameGroup :: n -> n -> Bool

instance NameLike (Name x) where
  sameGroup a b = group a == group b




newtype NameRaw = NameRaw{unNameRaw :: Name ()}
  deriving (NFData, Eq, Show, NameLike)

-- | A wrapped name containing a map of each processed token and the token
-- occurance count in otherData.
newtype NameTokenList = NameTokenList{unNameTokenList :: Name [Text]}
  deriving (NFData, Eq, NameLike)

tokenList :: NameTokenList -> [Text]
tokenList = otherData . unNameTokenList

-- | A wrapped name containing a map of each processed token and the token
-- occurance count in otherData.
newtype NameTokenCount = NameTokenCount{unNameTokenCount :: Name (Map Text Int)}
  deriving (NFData, Eq, NameLike)

tokens :: NameTokenCount -> Map Text Int
tokens = otherData . unNameTokenCount

-- | A wrapped name containing a norm and a map from each token to a count,
-- weight, and weight squared in otherData.
newtype NameNormed = NameNormed{unNameNormed :: Name NormWeights}
  deriving (NFData, Eq, Show, NameLike)

data NormWeights = NormWeights
  { _norm    :: !Double
  , _countWeights :: !(Map Text CountWeights)
  } deriving (Eq, Show)

-- | A type alias for the number of tokens occurance in a word,
-- the token weight, and the squared token weight.
type CountWeights = (Int, Double, Double)

norm :: NameNormed -> Double
norm = _norm . otherData . unNameNormed

countWeights :: NameNormed -> Map Text CountWeights
countWeights = _countWeights . otherData . unNameNormed


-- | A class for wrapped names
class NameRawLike a where
  toNameRaw :: a -> NameRaw

instance NameRawLike NameTokenCount where
  toNameRaw n = let n' = unNameTokenCount n 
                in  NameRaw $ n'{otherData = ()}
instance NameRawLike NameTokenList where
  toNameRaw n = let n' = unNameTokenList n 
                in NameRaw $ n'{otherData = ()}
instance NameRawLike NameNormed where
  toNameRaw n = let n' = unNameNormed n 
                in NameRaw $ n'{otherData = ()}


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
emptyName i n g = NameRaw
  Name{ idx        = i
      , name      = n
      , group     = g
      , otherData = ()
      }


-- | Encode the name tokens
encodeName  :: Bool           -- ^ Retain numeric characters
            -> (Text -> Text) -- ^ Encoding function
            -> NameRaw       -- ^ Source name
            -> NameTokenCount
encodeName retainNumeric encode nr = 
  NameTokenCount n{otherData = unCounter . countTokens . tokenList $ ntl}

    where

  ntl = encodeNameTokenList retainNumeric encode nr
  n   = unNameTokenList ntl


encodeNameTokenList  :: Bool          -- ^ Retain numeric characters
                     -> (Text -> Text) -- ^ Encoding function
                     -> NameRaw       -- ^ Source name
                     -> NameTokenList
encodeNameTokenList retainNumeric encode nr
  = NameTokenList $ n{otherData = tokenCounts}
  where
    tokenCounts = map (encode . toLower) . T.words . T.map filterLetters $ name
    filterLetters x = if charFilter x then x else ' '
    charFilter      = if retainNumeric then isAlphaNum else isLetter

    n@Name{..} = unNameRaw nr



-- | Compute the name norm score given a token weight map
normName :: TokenWeightMap -> NameTokenCount -> NameNormed
normName twm nt = NameNormed $
  n{ otherData =
    NormWeights{ _norm = sqrt $ scoreMap tw
               , _countWeights = tw
               }
  }
  where
    transformValues :: Text -> Int -> CountWeights -- (Int, Double, Double)
    transformValues k c = (c, w, w**2)
      where w = twm `findWeight` k

    tw :: Map Text CountWeights -- (Int, Double, Double)
    tw = mapWithKey transformValues (tokens nt)

    n@Name{..} = unNameTokenCount nt

-- | Remove a single token occurance. This would be nicer with lenses.
remove1TokenCount :: Text -> NameNormed -> NameNormed
remove1TokenCount t n = NameNormed $ nr{otherData = NormWeights _norm cw'}
  where
    nr  = unNameNormed n
    NormWeights{..} = otherData nr

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


-- | 
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
scoreMap :: Map Text CountWeights -> Double -- (Int, Double) -> Double
scoreMap = sqrt . foldrWithKey squareWeights 0
  where
    squareWeights :: Text -> CountWeights -> Double -> Double
    squareWeights _ (i, _, wsq) cum = cum + fromIntegral i * wsq


-- | Find the weight for a token, returning rarest token value if token is
-- not weighted
findWeight :: TokenWeightMap -> Text -> Double
findWeight TokenWeightMap{..} t =  findWithDefault rarestTokenValue t tokenWeights





-- | Application modes
data YenteMode = Cosine | Levenshtein



-- | A counter
newtype Counter i = Counter {unCounter :: Map Text i}
  deriving (Show, Functor)

emptyCounter :: Counter a
emptyCounter = Counter emptyMap

addText :: Counter Int -> Text -> Counter Int
addText c t = Counter . insertWith (+) t 1 $ unCounter c

countTokens :: [Text] -> Counter Int
countTokens = foldl' addText emptyCounter

