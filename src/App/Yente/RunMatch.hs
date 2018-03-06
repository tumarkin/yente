{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module App.Yente.RunMatch
  ( yente
  , yentl
  ) where

import qualified Data.ByteString.Lazy      as BSL
import           Data.Csv                  (encodeWith)
import           GHC.Conc                  (numCapabilities)
import           GHC.Prim
import           System.IO
import           Text.PhoneticCode.Phonix
import           Text.PhoneticCode.Soundex

import           App.Yente.Concurrent
import           App.Yente.Cosine
import           App.Yente.IO
import           App.Yente.Levenshtein
import           App.Yente.Prelude hiding (group)
import           App.Yente.Types



-- Entry into yente algorithm
yente ∷ YenteOptions → IO ()
yente yo@YenteOptions{..} = do
    (fromNameTL, toNameTL) <- readAndPreprocessNames yo

    let wts                = computeIDF toNamesTokenized
        fromNamesTokenized = toNameTokenCount <$> fromNameTL
        toNamesTokenized   = toNameTokenCount <$> toNameTL
        fromNamesWts       = map (normName wts) fromNamesTokenized
        toNamesWts         = map (normName wts) toNamesTokenized
        comparer           = compareNameListToNameCosine (misspellingPenalty matchingOptions)

    yenteG yo comparer fromNamesWts toNamesWts


yentl yo@YenteOptions{..} = do
    (fromNameTL, toNameTL) <- readAndPreprocessNames yo
    yenteG yo compareNameListToNameLev fromNameTL toNameTL

-- | Generic yente algorithm capable of handling generic name
-- transformation and comparison functions.
yenteG ∷ YenteOptions
       → ( (Vector (Name a), Name a) → Vector NameComparison ) -- | Comparison function
       → Vector (Name a)                                  -- | From names
       → Vector (Name a)                                  -- | To Names
       → IO ()
yenteG YenteOptions{..} compare fromNames toNames = do
    let MatchingOptions{..} = matchingOptions

    hPutStrLn stderr $
            unwords ["Matching from", show fromFile , "to", show toFile, "with", show numCapabilities, "cores"]

    -- matchWriter <- filepathToNameWriter fileFormat outputFile
    outHandle   <- openFile outputFile WriteMode
    hPutStr outHandle $
      if subgroupSearch then "from id, from name, to id, to name, group, score\n"
                        else "from id, from name, to id, to name, score\n"

    -- Compute the matches and save results ( select <<< compute weights <<< filter)
    let processName _ = return . selectResults outputOptions . compare . (subgroupFilterFcn toNames &&& id)
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
    subgroupFilterFcn = subgroupFilter (subgroupSearch matchingOptions)



nameEncoder ∷ PreprocessingOptions → NameRaw → NameTokenList
nameEncoder PreprocessingOptions{..} =
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


selectResults ∷ OutputOptions → Vector NameComparison → Vector NameComparison
selectResults OutputOptions{..} =
    selectBest includeTies numberOfResults . filterComparison minimumMatchScore


subgroupFilter ∷ (IsSequence seq, Element seq ~ Name b)
               ⇒ Bool → seq → Name a → seq
subgroupFilter True  ns n = filter (sameGroup n) ns
subgroupFilter False ns _ = ns

compareNameListToNameCosine ∷ Traversable t
                            ⇒ PenaltyFunctionCoefficient
                            → (t NameNormed, NameNormed)
                            → t NameComparison
compareNameListToNameCosine Nothing   (ns, n) = map (cosine n) ns                    -- No misspelling
compareNameListToNameCosine (Just pf) (ns, n) = map (cosineWithMispellings pf n) ns  -- Allow misspellings


compareNameListToNameLev ∷ Traversable t
                         ⇒ (t NameTokenList, NameTokenList)
                         → t NameComparison
compareNameListToNameLev (ns, n) = map (levenshtein n) ns



-- | Read and preprocess name
readAndPreprocessNames ∷ MonadIO m ⇒ YenteOptions → m (Vector NameTokenList, Vector NameTokenList)
readAndPreprocessNames YenteOptions{..} = do
    fromNameTL <- map (nameEncoder preprocessingOptions) <$> readNamesFile fromFile
    toNameTL   <- map (nameEncoder preprocessingOptions) <$> readNamesFile toFile
    return (fromNameTL, toNameTL)



-- select best possibly including ties
selectBest ∷ Bool → Int → Vector NameComparison → Vector NameComparison
selectBest _     _               (null → True) = empty
selectBest False matchesToOutput ncs           = take matchesToOutput . sortBy (flip compare) $ ncs
selectBest True  matchesToOutput ncs           = takeWhile (>= cutoff) sorted
    where
  cutoff ∷ NameComparison
  cutoff = fromMaybe (error "No cutoff") $ (last <$> fromNullable (take matchesToOutput sorted))
  sorted = sortBy (flip compare) ncs

filterComparison ∷ (IsSequence seq, Element seq ~ NameComparison)
                 ⇒ Double → seq → seq
filterComparison minScore
    = filter (\nc → score nc >= minScore)

