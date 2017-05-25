module App.Yente.Types.NameComparison
  ( NameComparison(..)
  ) where

import           Control.DeepSeq

import           App.Yente.Types.Name
import           App.Yente.Prelude

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

