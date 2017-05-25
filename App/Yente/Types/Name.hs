module App.Yente.Types.Name
    ( Name(..)
    , emptyName
    , sameGroup
    , encodeName
    , normName
    ) where

import           Control.DeepSeq
import           Data.Char                      (isLetter, isNumber, isSpace)
-- import Data.Text (Text, toLower)
-- import qualified Data.Map.Strict                as DM
import qualified Data.Text as T

import           App.Yente.Types.TokenWeightMap
import           App.Yente.Prelude hiding (group)


-- Name data type
data Name = Name
  { idx      :: !Text
  , name     :: !Text
  , tokens   :: ![Text]
  , group    :: Maybe Text
  , norm     :: Maybe Double
  , tokenWts :: Maybe [(Text, Double)]
  } deriving (Show, Eq)

instance NFData Name where
  rnf Name{ idx = i
          , name = n
          , tokens = ts
          , group = g
          , norm   = nrm
          , tokenWts = tws
          } =
    i `seq` n `seq` rnf ts `seq` g `seq` nrm `seq` rnf tws `seq` ()

-- Function to create empty name data from id and name strings
-- during cassava import
emptyName :: Text -- ^ Id
          -> Text -- ^ Name
          -> Maybe Text -- ^ Group
          -> Name
emptyName i n g = Name{ idx      = i
                    , name     = n
                    , tokens   = []
                    , group    = g
                    , norm     = Nothing
                    , tokenWts = Nothing
                    }

-- Check if two names have the same group
sameGroup :: Name -> Name -> Bool
sameGroup a b = group a == group b


-- Encode the name tokens
encodeName  :: Bool           -- ^ Retain numeric characters
            -> (Text -> Text) -- ^ Encoding function
            -> Name          -- ^ Source name
            -> Name
encodeName retainNumeric encodingFcn n
  = n{tokens   = map (encodingFcn . toLower) . T.words . T.map filterLetters $ name n}
  where
    filterLetters x = if charFilter x then x else ' '
    charFilter      = if retainNumeric
                      then \c -> or $ [isLetter, isNumber] <*> [c]
                      else isLetter


-- Compute the name norm score given a token weight map
normName :: TokenWeightMap -> Name -> Name
normName twm n = n{ norm     = Just (sqrt (twm `scoreTokenList` tokens n))
                  , tokenWts = Just tws
                  }
  where
    tws = [(x, twm `findWeight` x) | x <- tokens n]





