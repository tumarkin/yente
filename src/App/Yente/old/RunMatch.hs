{-# LANGUAGE RecordWildCards #-}

module App.Yente.RunMatch
  ( yente
  ) where

import           Control.Arrow ((&&&))
import           Data.Maybe                (isJust)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           GHC.Conc                  (numCapabilities)
import           System.IO hiding (putStrLn, print)
import qualified System.IO.Streams         as Streams
import           Text.PhoneticCode.Phonix
import           Text.PhoneticCode.Soundex

import           App.Yente.CLICmdArgs
import           App.Yente.Cosine
import           App.Yente.IO
import           App.Yente.Levenshtein
import           App.Yente.Parallel
import           App.Yente.Prelude
import           App.Yente.Types

preProcessingChunkSize = 50
comparisonChunkSize    = 20
comparisonWithMisspellingChunkSize = 1


yente :: YenteMode -> YenteOptions -> IO ()
yente ymode YenteOptions{..} = do

  putStrLn $ unwords ["Number of cores:", show numCapabilities]
  putStrLn $ unwords ["Matching from", show fromFile , "to", show toFile ]

  matchWriter <- case outputFile of
                  Nothing       -> handleToNameWriter fileFormat stdout
                  Just outfname -> filepathToNameWriter fileFormat outfname

  stdMirror   <- handleToNameWriter fileFormat stdout


  --- Build the list of potential matches and the cosine weights
  fromRawNames <- loadAndEncodeNameList fromFile
  toRawNames   <- loadAndEncodeNameList toFile

  let wts       = computeIDF toRawNames
      fromNames = normName wts <$> fromRawNames
      toNames   = normName wts <$> toRawNames

      -- fromNames = pMapChunk preProcessingChunkSize (normName wts) fromRawNames

      -- toNamesV :: Vector Name
      -- toNamesV  = V.fromList toNames

  -- Compute the matches ( select <<< compute weights <<< filter)
  let results = concatMap (selectionFcn . uncurry (compareNameListToName ymode misspellingPenalty wts) . (subgroupFilterFcn toNames &&& id)) fromNames

  -- -- Output the results
  mapM_ (\nc -> Streams.write (Just nc) matchWriter >> when outputRequested (Streams.write (Just nc) stdMirror)) results
  Streams.write Nothing matchWriter

    where

  --- Name processing function configuration
  loadAndEncodeNameList :: String -> IO [Name]
  loadAndEncodeNameList f = map (encodeName retainNumeric namePrepFcn) <$> loadNameList f

  namePrepFcn :: Text -> Text
  namePrepFcn    = letterLimitFcn . phoneticFcn

  phoneticFcn :: Text -> Text
  phoneticFcn    = case phoneticAlgorithm of
    Nothing -> id
    Just prePhonetic -> case downcaseString prePhonetic of
      -- "metaphone" -> metaphone
      "soundex"   -> T.pack . soundex True . T.unpack
      "phonix"    -> T.pack . phonix . T.unpack
      _           -> error $ "Phonetic algorithm " ++ prePhonetic ++ " not recognized"

  letterLimitFcn :: Text -> Text
  letterLimitFcn = maybe id T.take maxTokenLength

  -- Output configuration
  outputRequested = isJust outputFile
  fileFormat      = maybe defaultFileFormat getFileFormat outputFile

  --- Matching selection function configuration
  selectionFcn = selectBest includeTies numberOfResults . filterComparison minimumMatchScore


  -- Subgroup selection function
  subgroupFilterFcn = if subgroupSearch then subgroupFilter
                      else subgroupPassThru

  subgroupFilterFcnV = if subgroupSearch then subgroupFilterV
                      else subgroupPassThruV

subgroupFilter :: [Name] -> Name -> [Name]
subgroupFilter ns n = filter (sameGroup n) ns

subgroupPassThru :: [Name] -> Name -> [Name]
subgroupPassThru ns _ = ns


subgroupFilterV :: Vector Name -> Name -> Vector Name
subgroupFilterV ns n = V.filter (sameGroup n) ns

subgroupPassThruV :: Vector Name -> Name -> Vector Name
subgroupPassThruV ns _ = ns







compareNameListToName :: YenteMode
                      -> Maybe Double
                      -> TokenWeightMap
                      -> [Name]
                      -> Name
                      -> [ NameComparison ]
compareNameListToName Cosine Nothing   wts ns n = pMapChunk comparisonChunkSize (cosine wts n) ns                    -- No misspelling
compareNameListToName Cosine (Just pf) wts ns n = pMapChunk comparisonWithMisspellingChunkSize (cosineWithMispellings pf wts n) ns  -- Allow misspellings
compareNameListToName Levenshtein _    _   ns n = pMapChunk comparisonChunkSize (levenshtein n) ns

compareNameListToNameT :: Traversable t
                      => YenteMode
                      -> Maybe Double
                      -> TokenWeightMap
                      ->  t Name
                      -> Name
                      -> t NameComparison
compareNameListToNameT Cosine Nothing   wts ns n = cosine wts n <$> ns                    -- No misspelling
compareNameListToNameT Cosine (Just pf) wts ns n = cosineWithMispellings pf wts n <$> ns  -- Allow misspellings
compareNameListToNameT Levenshtein _    _   ns n = levenshtein n <$> ns



-- select best possibly including ties
selectBest :: Bool -> Int -> [NameComparison] -> [NameComparison]
selectBest _     _               []  = []
selectBest False matchesToOutput ncs = take matchesToOutput . sortBy (flip compare) $ ncs
selectBest True  matchesToOutput ncs = takeWhile (>= cutoff) sorted
  where
    cutoff = last . take matchesToOutput $ sorted
    sorted = sortBy (flip compare) ncs

-- selectBestV :: Bool 
--             -> Int 
--             -> Vector NameComparison 
--             -> Vector NameComparison
-- -- selectBestT _     _               []  = []
-- selectBestV False matchesToOutput ncs = take matchesToOutput . sortBy (flip compare) $ ncs
-- selectBestV True  matchesToOutput ncs = takeWhile (>= cutoff) sorted
--   where
--     cutoff = last . take matchesToOutput $ sorted
--     sorted = sortBy (flip compare) ncs

filterComparison :: Double -> ([NameComparison] -> [NameComparison])
filterComparison minScore
    = filter (\nc -> score nc >= minScore)










