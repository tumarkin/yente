{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module App.Yente.Match
  ( module X
  , YenteOptions(..)
  , yenteG
  ) where

import qualified Data.ByteString.Lazy      as BSL
import           Data.Csv                  (encodeWith)
import           GHC.Conc                  (numCapabilities)
import           GHC.Prim
import           System.IO (hPutStr, hPutStrLn, openFile)

import           App.Yente.Match.Compare      as X
import           App.Yente.Match.Output     as X
import           App.Yente.Match.PreProcess as X
import           App.Yente.Core

data YenteOptions = YenteOptions
  { fromFile            ∷ String
  , toFile              ∷ String
  , preprocessingConfig ∷ PreprocessingConfig
  , matchConfig         ∷ MatchConfig
  , outputConfig        ∷ OutputConfig
  , outputFile          ∷ FilePath
  } deriving (Show) --  Data) , Typeable)


-- | Generic yente algorithm capable of handling generic name
-- transformation and comparison functions.
yenteG ∷ YenteOptions
       → (Name a → Name a → NameComparison ) -- | Comparison function
       → Vector (Name a)                     -- | From names
       → Vector (Name a)                     -- | To Names
       → IO ()
yenteG YenteOptions{..} compareSingle fromNames toNames = do
    let MatchConfig{..} = matchConfig

    hPutStrLn stderr $
            unwords ["Matching from", show fromFile , "to", show toFile, "with", show numCapabilities, "cores"]

    outHandle   <- openFile outputFile WriteMode
    hPutStr outHandle $
      if subgroupSearch then "from id, from name, to id, to name, group, score\n"
                        else "from id, from name, to id, to name, score\n"

    -- Compute the matches and save results ( select <<< compute weights <<< filter)
    let processName _ = return . selectResults outputConfig . compare . (subgroupFilterFcn toNames &&& id)

       -- compare ∷ Vector (Name b) → Name a → Vector NameComparison
        compare (ns, n) = map (compareSingle n) ns

        writeNames ∷ Vector NameComparison → IO ()
        writeNames = mapM_ (writeName subgroupSearch)

        writeName ∷ Bool → NameComparison → IO ()
        writeName True nc@NameComparison{..} = BSL.hPut outHandle commaData
            where
              commaData = encodeWith eo [ (idx fromName, name fromName, idx toName
                                          ,name toName, group fromName, score)]

        writeName False nc@NameComparison{..} = BSL.hPut outHandle commaData
            where
              commaData = encodeWith eo [(idx fromName, name fromName, idx toName, name toName, score)]

    runConcurrent numCapabilities threadConfig processName writeNames fromNames


    -- Close the output stream
    hClose outHandle


  where
    threadConfig ∷ IO ()
    threadConfig = return ()

    -- Output configuration
    eo = getEncodeOptions outputFile

    -- Subgroup selection function
    subgroupFilterFcn = subgroupFilter (subgroupSearch matchConfig)





subgroupFilter ∷ (IsSequence seq, Element seq ~ Name b)
               ⇒ Bool → seq → Name a → seq
subgroupFilter True  ns n = filter (sameGroup n) ns
subgroupFilter False ns _ = ns

sameGroup ∷ Name a
          → Name b
          → Bool
sameGroup na nb = group na == group nb







