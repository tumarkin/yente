module App.Yente.Types.Name
    ( Name(..)
    , emptyName
    , sameGroup
    , encodeName
    , normName
    ) where

import Control.Applicative
import Control.DeepSeq
import Data.Char (isLetter, isSpace, toLower, isNumber)
import qualified Data.Map.Strict as DM

import App.Yente.Types.TokenWeightMap


-- Name data type
data Name = Name 
  { idx      :: !String
  , name     :: !String
  , tokens   :: ![String]
  , group    :: Maybe String
  , norm     :: Maybe Double
  , tokenWts :: Maybe [(String, Double)] 
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
emptyName :: String -- ^ Id
          -> String -- ^ Name
          -> Maybe String -- ^ Group
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
encodeName  :: Bool               -- ^ Retain numeric characters
            -> (String -> String) -- ^ Encoding function
            -> Name               -- ^ Source name
            -> Name
encodeName retainNumeric encodingFcn n
    = n{tokens   = map (encodingFcn . convertCase) . words . map filterLetters $ name n
       }
    where 
  convertCase     = map toLower
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





