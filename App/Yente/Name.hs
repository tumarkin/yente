module App.Yente.Name 
            ( Name(..)
            , emptyName
            , encodeName
            , normName
            ) where

import Control.DeepSeq
import Data.Char (isLetter, isSpace, toLower)
import qualified Data.Map.Strict as DM

import App.Yente.TokenWeightMap


-- Name data type
data Name = Name 
  { idx      :: !String
  , name     :: !String
  , tokens   :: ![String]
  , norm     :: Maybe Double
  , tokenWts :: Maybe [(String, Double)] 
  } deriving (Show, Eq)

instance NFData Name where
  rnf Name{ idx = i
          , name = n
          , tokens = ts
          , norm   = nrm
          , tokenWts = tws
          } =
    i `seq` n `seq` rnf ts `seq` nrm `seq` rnf tws `seq` ()





-- Function to create empty name data from id and name strings
-- during cassava import
emptyName :: String -- ^ Id
          -> String -- ^ Name
          -> Name
emptyName i n = Name{ idx      = i
                    , name     = n
                    , tokens   = []
                    , norm     = Nothing
                    , tokenWts = Nothing
                    }


-- Encode the name tokens
encodeName  :: (String -> String) -- ^ Encoding function
            -> Name               -- ^ Source name
            -> Name
encodeName encodingFcn n
    = n{tokens   = map (encodingFcn . convertCase) . words . map filterLetters $ name n
       }
    where 
  convertCase     = map toLower
  filterLetters x = if isLetter x then x else ' '




normName :: TokenWeightMap -> Name -> Name
normName twm n = n{ norm     = Just (sqrt (twm `scoreTokenList` tokens n)) 
                  , tokenWts = Just tws
                  }
    where
  tws = [(x, twm `findWeight` x) | x <- tokens n]




