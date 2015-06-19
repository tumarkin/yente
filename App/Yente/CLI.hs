module App.Yente.CLI(optFromFile
                    , optToFile
                    , prePhonetic
                    , preNLetters
                    , matchNOps
                    , misspellingPenalty
                    , resultsNum 
                    , resultsTies 
                    , optResultsCutoff 
                    , optOutputFile 
                    ) where

import System.Console.CmdTheLine

optFromFile :: Term (Maybe String)
optFromFile = value $ pos 0 Nothing posInfo { posName = "FROM-FILE" }

optToFile :: Term (Maybe String)
optToFile = value $ pos 1 Nothing posInfo { posName = "TO-FILE" }


-- From Cmdtheline.Term.hs (To get the word default instead of absent)
{-defaultOpt (Just Pager) Nothing (optInfo ["help"])-}
{-Term ais lookup = value $ defaultOpt (Just Pager) Nothing (optInfo ["help"])-}
{-        { optSec  = section-}
{-            , optName = "FMT"-}
{-                , optDoc  = doc-}
{-        }-}

--data PhoneticAlgorithms 
--    = SoundexAlg
--    | MetaphoneAlg
--    deriving (Show, Eq)

prePhonetic :: Term String
prePhonetic = value $ opt "None" (optInfo [ "phonetic-algorithm", "phonetic", "p" ])
     { optDoc  = "An optional phonetic algorithm to transform the name. Supported algorithms are Metaphone, Phonix and SoundEx. (TO DO: METAPHONE)"
     , optName = "Algorithm"
     , optSec  = "PRE-PROCESSING OPTIONS"
     }

preNLetters :: Term (Maybe Int)
preNLetters = value $ opt Nothing (optInfo ["max-token-length"])
    { optDoc = "Trim each word to have a maximum number of letters. Can be useful for phonetic based matching." 
     , optSec = "PRE-PROCESSING OPTIONS"
     }

matchNOps :: Term Int
matchNOps = value $ opt 0 (optInfo ["matching-edit-distance", "e"])
    { optDoc = "The maximum allowable Damerau-Levenshtein edit distance when determining whether two tokens match. (TO DO: Not implemented)"
    , optSec = "MATCHING OPTIONS"
    }

misspellingPenalty :: Term (Maybe String) 
misspellingPenalty = value $ opt Nothing (optInfo ["misspelling-penalty"])
    { optDoc = "The misspelling penalty factor. (Empty to not allow misspellings, otherwise percent correct letters raised to factor serves as penalty)" 
    , optSec = "MATCHING OPTIONS"
    }

resultsNum :: Term Int
resultsNum = value $ opt 1 (optInfo [ "number-of-results", "n" ])
     { optDoc = "The number of results to output." 
     , optSec = "OUTPUT OPTIONS"
     }

resultsTies :: Term Bool
resultsTies = value $ opt True (optInfo [ "ties-in-results"])
     { optDoc = "Include ties in output." 
     , optSec = "OUTPUT OPTIONS"
     }

optResultsCutoff :: Term Double
optResultsCutoff = value $ opt 0.001 (optInfo ["minimum-match-score", "m"]) 
    { optDoc = "The minimum score required to be considered a match. At a minimum this should be slightly greater than 0 to allow the algorithm to eliminate non-matches (score = 0) when comparing floating point numbers. (TO DO: Fix, currently non-matches skipped)"
    , optSec = "OUTPUT OPTIONS"
    }

optOutputFile :: Term (Maybe String)
optOutputFile = value $ opt Nothing (optInfo ["output-file","o"])
    { optDoc = "Output the results to file. (Default is a tab delimited file. Files with .csv extension will be comma delimited." 
    , optSec = "OUTPUT OPTIONS"
    }



