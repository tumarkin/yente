module App.Yente.Match.PreProcess
  ( PreprocessingConfig(..)
  , PhoneticAlgorithm(..)
  , tokenizeNames
  , nameEncoder
  ) where

import           Data.Char                 (isAlphaNum, isLetter)
import           Text.PhoneticCode.Phonix
import           Text.PhoneticCode.Soundex
import qualified Data.Map.Strict as M

import           App.Yente.Core

--------------------------------------------------------------------------------
-- Configuration                                                              --
--------------------------------------------------------------------------------
data PreprocessingConfig = PreprocessingConfig
  { phoneticAlgorithm ∷ !(Maybe PhoneticAlgorithm)
  , retainNumeric     ∷ !Bool
  , retainUnicode     ∷ !Bool
  , maxTokenLength    ∷ !(Maybe Int)
  } deriving (Show)

data PhoneticAlgorithm
  = Phonix
  | Soundex
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- External preprocessing functions                                           --
--------------------------------------------------------------------------------
tokenizeNames ∷ PreprocessingConfig
              → Vector (Name a)
              → Vector (Name a)
              → (Vector (Name NormWeights), Vector (Name NormWeights))
tokenizeNames preCfg fromNamesRaw toNamesRaw = 
    (fromNamesWts, toNamesWts)
  where
    wts                = computeIdf toNamesTokenized
    fromNameTL         = map (nameEncoder preCfg) fromNamesRaw
    toNameTL           = map (nameEncoder preCfg) toNamesRaw
    fromNamesTokenized = toNameTokenCounter <$> fromNameTL
    toNamesTokenized   = toNameTokenCounter <$> toNameTL
    fromNamesWts       = map (normName wts) fromNamesTokenized
    toNamesWts         = map (normName wts) toNamesTokenized

--------------------------------------------------------------------------------
-- Internal preprocessing functions                                           --
--------------------------------------------------------------------------------
nameEncoder ∷ PreprocessingConfig → Name a → Name TokenList
nameEncoder PreprocessingConfig{..} =
    encodeNameTokenList retainNumeric namePrepFcn
  where
    namePrepFcn ∷ Text → Text
    namePrepFcn = letterLimitFcn . phoneticFcn . unicodeFcn

    phoneticFcn ∷ Text → Text
    phoneticFcn = case phoneticAlgorithm of
      Just Soundex → cs . soundex True . cs
      Just Phonix  → cs . phonix . cs
      _            → id

    unicodeFcn ∷ Text → Text
    unicodeFcn = if retainUnicode then id else unidecode

    letterLimitFcn ∷ Text → Text
    letterLimitFcn = maybe id take maxTokenLength

encodeNameTokenList  ∷ Bool          -- ^ Retain numeric characters
                     → (Text → Text) -- ^ Encoding function
                     → Name a        -- ^ Source name
                     → Name TokenList
encodeNameTokenList retainNumeric encode n
    = tokenCounts <$ n
  where
    tokenCounts = map (encode . toLower) . words . omap filterLetters $ name n

    filterLetters ∷ Char → Char
    filterLetters x = if desiredChar x then x else ' '

    desiredChar ∷ Char → Bool
    desiredChar = if retainNumeric then isAlphaNum else isLetter


toNameTokenCounter ∷ Name TokenList → Name TokenCounter
toNameTokenCounter = fmap countTokens


-- | Compute the name norm score given a token weight map
normName ∷ TokenWeightMap → Name TokenCounter → Name NormWeights
normName twm = fmap normWeights
  where
    normWeights ∷ TokenCounter → NormWeights
    normWeights (TokenCounter tc) =
        NormWeights{ _norm = sqrt $ sumSquareWeightMap tw
                   , _countWeights = tw
                   }
      where
        transformValues ∷ Text → Int → CountWeights
        transformValues k c = (c, w, w**2)
          where w = twm `findWeight` k

        tw ∷ Map Text CountWeights
        tw = mapWithKey transformValues tc


computeIdf ∷ Vector (Name TokenCounter) -- ^ Traversible names
           → TokenWeightMap
computeIdf ns = TokenWeightMap
    { tokenWeights     = tw
    , rarestTokenValue = maximum weights
    }
  where

    tw ∷ Map Text Double
    tw = scoreFcn <$> unTokenCounter tokenCounts

    corpsize ∷ Double
    corpsize = fromIntegral $ length ns

    uniqueNameTokens ∷ Vector [Text]
    uniqueNameTokens = ordNub . M.keys . unTokenCounter . nameData <$> ns

    tokenCounts ∷ TokenCounter
    tokenCounts = countTokens $ concat uniqueNameTokens

    scoreFcn ∷ Int → Double
    scoreFcn i = log(corpsize/fromIntegral i)

    weights = fromMaybe (error "No tokens or weights found in TO match file")
              (fromNullable $ elems tw)

countTokens ∷ [Text] → TokenCounter
countTokens = foldl' addToken emptyCounter

