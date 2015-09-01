--
import Control.Applicative
import Control.Monad
import Data.Char (toLower)
import Data.List
import Data.List.Split
import Data.Map.Strict ((!))
import Data.Maybe (fromJust, isJust)
import GHC.Conc (numCapabilities)
import System.Console.CmdArgs
-- import System.Console.CmdTheLine
import System.Environment
import System.Exit (exitSuccess)
import Text.PhoneticCode.Phonix
import Text.PhoneticCode.Soundex
-- import Text.PhoneticCode.Metaphone
import qualified Data.ByteString.Lazy as BS
{-import qualified Data.Map.Strict as DM-}
import qualified Data.Csv as CSV -- cassava
import qualified Data.Vector as V
import System.IO

import qualified System.IO.Streams as Streams

-- import System.IO.Streams.Csv -- cassava-streaming


import Debug.Trace

import App.Yente.CLICmdArgs
import App.Yente.Name
import App.Yente.NameComparison
import App.Yente.TokenWeightMap
import App.Yente.Cosine
import App.Yente.IO
import App.Yente.Parallel



preProcessingChunkSize = 50
comparisonChunkSize    = 20
comparisonWithMisspellingChunkSize = 1


                  


yente :: YenteOptions -> IO ()
yente yopts = do



  putStrLn $ "Number of cores: " ++ show numCapabilities
  putStrLn $ unwords ["Matching from", show (fromFile yopts), "to", show (toFile yopts)]

  matchWriter <- case outFile of 
                  Nothing       -> handleToNameWriter fileFormat stdout 
                  Just outfname -> filepathToNameWriter fileFormat outfname

  stdMirror   <- handleToNameWriter fileFormat stdout 


  --- Build the list of potential matches and the cosine weights
  fromRawNames <- filepathToNames ( fromFile yopts)         -- Read in names
                  >>= (return . map (encodeName namePrepFcn))  -- Encode names
  toRawNames   <- filepathToNames ( toFile yopts)   -- Read in names 
                  >>= (return . pMapChunk preProcessingChunkSize (encodeName namePrepFcn)) -- Encode in parallel
        
  let wts             = computeWeights (map tokens toRawNames)

      fromNames = pMapChunk preProcessingChunkSize (normName wts) fromRawNames
      toNames   = map (normName wts) toRawNames

  let results = concatMap (selectionFcn . compareNameListToName misspellingFactor wts toNames) $ fromNames

  mapM_ (\nc -> Streams.write (Just nc) matchWriter >> when outputRequested (Streams.write (Just nc) stdMirror)) $ results
  Streams.write Nothing matchWriter

    where 
  
  --- Name processing function configuration
  namePrepFcn    = letterLimitFcn . phoneticFcn 

  phoneticFcn    = case (phonetic_algorithm yopts) of
                      Nothing -> id
                      Just prePhonetic -> case (downcase prePhonetic) of 
                                            -- "metaphone" -> metaphone
                                            "soundex"   -> soundex True
                                            "phonix"    -> phonix
                                            _           -> error $ "Phonetic algorithm " ++ (prePhonetic) ++ " not recognized"

  letterLimitFcn = case (max_token_length yopts) of
                      Just n  -> take n
                      Nothing -> id

--   -- Output configuration
  outFile = output_file yopts
  outputRequested = isJust outFile
  fileFormat      = case outFile of
                      Nothing       -> defaultFileFormat
                      Just outfname -> getFileFormat outfname

  -- Matching configuration
  misspellingFactor = misspelling_penalty yopts 

  --- Output selection function configuration
  selectionFcn = selectBest matchesWithTies matchesToOutput . filterComparison matchMinimumScore

  --- Other configuration
  matchesToOutput = number_of_results yopts
  matchesWithTies = include_ties yopts
  matchMinimumScore = minimum_match_score yopts








compareNameListToName :: Maybe Double -> TokenWeightMap -> [Name] -> Name -> [ NameComparison ]
compareNameListToName Nothing wts ns n   = pMapChunk comparisonChunkSize (cosine wts n) ns                    -- No misspelling
compareNameListToName (Just pf) wts ns n = pMapChunk comparisonWithMisspellingChunkSize (cosineWithMispellings pf wts n) ns  -- Allow misspellings



-- select best possibly including ties
selectBest :: Bool -> Int -> [NameComparison] -> [NameComparison]
selectBest _     _               []  = []
selectBest False matchesToOutput ncs = take matchesToOutput . reverse . sort $ ncs
selectBest True  matchesToOutput ncs = takeWhile (>= cutoff) sorted
  where
    cutoff = last . take matchesToOutput $ sorted
    sorted = reverse . sort $ ncs


filterComparison :: Double -> ([NameComparison] -> [NameComparison])
filterComparison minScore 
    = filter (\nc -> score nc >= minScore)


downcase :: String -> String
downcase = map toLower






main = yente =<< parseCLI

-- --
-- main :: IO ()
-- main = run ( term, termInfo )

