{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Yente.Types (
  -- * Names
    Name(..)
  , emptyName
  , sameGroup
  , encodeName
  , normName
  , NameComparison(..)

  -- * Token weight maps
  , TokenWeightMap(..)
  , scoreTokenList
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
data Name = Name
  { idx      :: !Text
  , name     :: !Text
  , group    :: !(Maybe Text)
  , tokens   :: ![Text]
  , norm     :: !(Maybe Double)
  , tokenWts :: !(Maybe [(Text, Double)])
  } deriving (Show, Eq)

instance NFData Name where
  rnf Name{ idx      = i
          , name     = n
          , group    = g
          , tokens   = ts
          , norm     = nrm
          , tokenWts = tws
          } =
    i `seq` n `seq` g `seq` rnf ts `seq` nrm `seq` rnf tws `seq` ()
 

-- | Function to create empty name data from id and name strings
-- during cassava import
emptyName :: Text       -- ^ Id
          -> Text       -- ^ Name
          -> Maybe Text -- ^ Group
          -> Name
emptyName i n g = Name{ idx      = i
                    , name     = n
                    , group    = g
                    , tokens   = []
                    , norm     = Nothing
                    , tokenWts = Nothing
                    }

-- | Check if two names have the same group
sameGroup :: Name -> Name -> Bool
sameGroup a b = group a == group b

-- | Encode the name tokens
encodeName  :: Bool           -- ^ Retain numeric characters
            -> (Text -> Text) -- ^ Encoding function
            -> Name          -- ^ Source name
            -> Name
encodeName retainNumeric encode n@Name{..}
  = n{tokens   = map (encode . toLower) . T.words . T.map filterLetters $ name}
  where
    filterLetters x = if charFilter x then x else ' '
    charFilter      = if retainNumeric then isAlphaNum else isLetter


-- | Compute the name norm score given a token weight map
normName :: TokenWeightMap -> Name -> Name
normName twm n = n{ norm     = Just (sqrt (twm `scoreTokenList` tokens n))
                  , tokenWts = Just tws
                  }
  where
    tws = [(x, twm `findWeight` x) | x <- tokens n]


-- | Name comparisons
data NameComparison = NameComparison
  { fromComparison :: !Name
  , toComparison   :: !Name
  , score          :: !Double
  } deriving (Show, Eq)

instance Ord NameComparison where
  compare = compare `on` score

instance NFData NameComparison where
  rnf NameComparison{
        fromComparison = fc
      , toComparison   = tc
      , score          = s
      } = rnf fc `seq` rnf tc `seq` s `seq` ()


data TokenWeightMap = TokenWeightMap
  { tokenWeights     :: !(Map Text Double)
  , rarestTokenValue :: !Double
  } deriving (Show, Eq)


computeIDF :: [Name] -- ^ List of names
                  -> TokenWeightMap
computeIDF ns =
  TokenWeightMap{ tokenWeights     = tw
                , rarestTokenValue = maximum .  elems $ tw
                }
  where
    tw               = unCounter $ scoreFcn <$> tokenCounts
    corpsize         = fromIntegral $ length ns
    unqiueNameTokens = nub . tokens <$> ns
    tokenCounts      = countTokens $ concat unqiueNameTokens

    scoreFcn :: Int -> Double
    scoreFcn i = log(corpsize/fromIntegral i)


-- | Score a list of words with tf
scoreTokenList :: TokenWeightMap -> [Text] -> Double
scoreTokenList _   []       = 0
scoreTokenList twm wordlist = sum [(twm `findWeight` x )**2 | x <- wordlist ]
-- scoreTokenList twm wordlist = sum $ findWeightSquared twm <$> wordlist -- [(twm `findWeight` x )**2 | x <- wordlist ]

-- | Find the weight for a token, returning rarest token value if token is
-- not weighted
findWeight :: TokenWeightMap -> Text -> Double
findWeight TokenWeightMap{..} t =  findWithDefault rarestTokenValue t tokenWeights

-- | Find the weight for a token, returning rarest token value if token is
-- not weighted
findWeightSquared :: TokenWeightMap -> Text -> Double
findWeightSquared twn t =  findWeight twn t ** 2





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

