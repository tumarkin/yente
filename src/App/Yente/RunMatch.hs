{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Yente.RunMatch
  ( yente
  ) where

import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           GHC.Conc                  (numCapabilities)
import           System.IO                 hiding (print, putStrLn)
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




-- Entry into yente algorithm
yente :: YenteMode -> YenteOptions -> IO ()
yente Cosine yo@YenteOptions{..} = yenteG trans comp yo
  where
    trans :: NamePrepFcn -> ([NameRaw], [NameRaw]) -> ([NameNormed], [NameNormed])
    trans namePrepFcn (fn, tn) = (map (normName wts) fromNamesTokenized, map (normName wts) toNamesTokenized)
      where
        wts                = computeIDF toNamesTokenized
        fromNamesTokenized = map (encodeName retainNumeric namePrepFcn) fn
        toNamesTokenized   = map (encodeName retainNumeric namePrepFcn) tn


    comp :: [NameNormed] -> NameNormed -> [NameComparison]
    comp  = compareNameListToNameCosine misspellingPenalty



yente Levenshtein yo@YenteOptions{..} = yenteG trans comp yo
  where
    trans :: NamePrepFcn -> ([NameRaw], [NameRaw]) -> ([NameTokenList], [NameTokenList])
    trans namePrepFcn (fn, tn) = (map (encodeNameTokenList retainNumeric namePrepFcn) fn, map (encodeNameTokenList retainNumeric namePrepFcn) tn)

    comp :: [NameTokenList] -> NameTokenList -> [NameComparison]
    comp  = compareNameListToNameLev


type NamePrepFcn = Text -> Text


-- | Generic yente algorithm capable of handling generic name
-- transformation and comparison functions.
yenteG :: NameLike a 
      => (NamePrepFcn -> ([NameRaw], [NameRaw]) -> ([a], [a]))  -- | Transformer of raw names
      -> ([a] -> a -> [NameComparison])          -- | Comparison function
      -> YenteOptions
      -> IO ()
yenteG transform comp YenteOptions{..} = do
    putStrLn $ unwords ["Number of cores:", show numCapabilities]
    putStrLn $ unwords ["Matching from", show fromFile , "to", show toFile ]

    matchWriter <- case outputFile of
                    Nothing       -> handleToNameWriter fileFormat stdout
                    Just outfname -> filepathToNameWriter fileFormat outfname

    stdMirror   <- handleToNameWriter fileFormat stdout


    --- Build the list of potential matches and the cosine weights
    fromNameRaw <- loadNameList fromFile
    toNameRaw   <- loadNameList toFile

    let (fromNames, toNames) = transform namePrepFcn (fromNameRaw, toNameRaw)

    -- Compute the matches ( select <<< compute weights <<< filter)
    let results = concatMap (selectionFcn . uncurry comp . (subgroupFilterFcn toNames &&& id)) fromNames

    -- Output the results
    mapM_ (\nc -> Streams.write (Just nc) matchWriter >> when outputRequested (Streams.write (Just nc) stdMirror)) results
    Streams.write Nothing matchWriter

  where

    namePrepFcn :: NamePrepFcn
    namePrepFcn    = letterLimitFcn . phoneticFcn . unicodeFcn

    phoneticFcn :: Text -> Text
    phoneticFcn    = case phoneticAlgorithm of
      Nothing -> id
      Just prePhonetic -> case downcaseString prePhonetic of
        -- "metaphone" -> metaphone
        "soundex"   -> T.pack . soundex True . T.unpack
        "phonix"    -> T.pack . phonix . T.unpack
        _           -> error $ "Phonetic algorithm " ++ prePhonetic ++ " not recognized"

    unicodeFcn :: Text -> Text
    unicodeFcn = if retainUnicode then id else unidecode

    letterLimitFcn :: Text -> Text
    letterLimitFcn = maybe id T.take maxTokenLength

    -- Output configuration
    outputRequested = isJust outputFile
    fileFormat      = maybe defaultFileFormat getFileFormat outputFile

    --- Matching selection function configuration
    selectionFcn = selectBest includeTies numberOfResults . filterComparison minimumMatchScore


    -- Subgroup selection function
    subgroupFilterFcn = if subgroupSearch then subgroupFilter else subgroupPassThru



-- yente :: YenteMode -> YenteOptions -> IO ()
-- yente ymode YenteOptions{..} = do

--   putStrLn $ unwords ["Number of cores:", show numCapabilities]
--   putStrLn $ unwords ["Matching from", show fromFile , "to", show toFile ]

--   matchWriter <- case outputFile of
--                   Nothing       -> handleToNameWriter fileFormat stdout
--                   Just outfname -> filepathToNameWriter fileFormat outfname

--   stdMirror   <- handleToNameWriter fileFormat stdout


--   --- Build the list of potential matches and the cosine weights
--   fromNamesTokenized <- loadAndEncodeNameList fromFile
--   toNamesTokenized   <- loadAndEncodeNameList toFile

--   let wts       = computeIDF toNamesTokenized
--       fromNames = normName wts <$> fromNamesTokenized
--       toNames   = normName wts <$> toNamesTokenized

--       -- toNamesV :: Vector Name
--       -- toNamesV  = V.fromList toNames

--   -- Compute the matches ( select <<< compute weights <<< filter)
--   let results = concatMap (selectionFcn . uncurry (compareNameListToName ymode misspellingPenalty) . (subgroupFilterFcn toNames &&& id)) fromNames

--   -- -- Output the results
--   mapM_ (\nc -> Streams.write (Just nc) matchWriter >> when outputRequested (Streams.write (Just nc) stdMirror)) results
--   Streams.write Nothing matchWriter

--   return ()


--     where

--   --- Name processing function configuration
--   loadAndEncodeNameList :: String -> IO [NameTokenCount]
--   loadAndEncodeNameList f = map (encodeName retainNumeric namePrepFcn) <$> loadNameList f

--   namePrepFcn :: Text -> Text
--   namePrepFcn    = letterLimitFcn . phoneticFcn

--   phoneticFcn :: Text -> Text
--   phoneticFcn    = case phoneticAlgorithm of
--     Nothing -> id
--     Just prePhonetic -> case downcaseString prePhonetic of
--       -- "metaphone" -> metaphone
--       "soundex"   -> T.pack . soundex True . T.unpack
--       "phonix"    -> T.pack . phonix . T.unpack
--       _           -> error $ "Phonetic algorithm " ++ prePhonetic ++ " not recognized"

--   letterLimitFcn :: Text -> Text
--   letterLimitFcn = maybe id T.take maxTokenLength

--   -- Output configuration
--   outputRequested = isJust outputFile
--   fileFormat      = maybe defaultFileFormat getFileFormat outputFile

--   --- Matching selection function configuration
--   selectionFcn = selectBest includeTies numberOfResults . filterComparison minimumMatchScore


--   -- Subgroup selection function
--   subgroupFilterFcn = if subgroupSearch then subgroupFilter
--                       else subgroupPassThru

--   subgroupFilterFcnV = if subgroupSearch then subgroupFilterV
--                       else subgroupPassThruV

subgroupFilter :: NameLike a => [a] -> a -> [a]
subgroupFilter ns n = filter (sameGroup n) ns

subgroupPassThru :: NameLike a => [a] -> a -> [a]
subgroupPassThru ns _ = ns


-- subgroupFilterV :: Vector (NameNormed) -> NameNormed -> Vector (NameNormed)
-- subgroupFilterV ns n = V.filter (sameGroup n) ns

-- subgroupPassThruV :: Vector (NameNormed) -> NameNormed -> Vector (NameNormed)
-- subgroupPassThruV ns _ = ns



compareNameListToNameCosine :: Maybe Double
                            -> [NameNormed]
                            -> NameNormed
                            -> [NameComparison]
compareNameListToNameCosine Nothing   ns n = pMapChunk comparisonChunkSize (cosine n) ns -- No misspelling
compareNameListToNameCosine (Just pf) ns n = pMapChunk comparisonWithMisspellingChunkSize (cosineWithMispellings pf n) ns  -- Allow misspellings


compareNameListToNameLev :: [NameTokenList]
                         -> NameTokenList
                         -> [NameComparison]
compareNameListToNameLev ns n = pMapChunk comparisonChunkSize (levenshtein n) ns

-- compareNameListToNameT :: Traversable t
--                       => YenteMode
--                       -> Maybe Double
--                       -> TokenWeightMap
--                       ->  t Name
--                       -> Name
--                       -> t NameComparison
-- compareNameListToNameT Cosine Nothing   wts ns n = cosine wts n <$> ns                    -- No misspelling
-- compareNameListToNameT Cosine (Just pf) wts ns n = cosineWithMispellings pf wts n <$> ns  -- Allow misspellings
-- compareNameListToNameT Levenshtein _    _   ns n = levenshtein n <$> ns



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
--             -> Vector (NameComparison n)
--             -> Vector (NameComparison n)
-- -- selectBestT _     _               []  = []
-- selectBestV False matchesToOutput ncs = take matchesToOutput . sortBy (flip compare) $ ncs
-- selectBestV True  matchesToOutput ncs = takeWhile (>= cutoff) sorted
--   where
--     cutoff = last . take matchesToOutput $ sorted
--     sorted = sortBy (flip compare) ncs

filterComparison :: Double -> ([NameComparison] -> [NameComparison])
filterComparison minScore
    = filter (\nc -> score nc >= minScore)










