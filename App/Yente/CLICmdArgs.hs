{-# Language DeriveDataTypeable #-}
module App.Yente.CLICmdArgs 
  ( YenteOptions(..)
  , yenteOptions
  , parseCLI
  ) where

import System.Console.CmdArgs.Implicit
import Data.Maybe (isJust)


-- Use to debug CLI
parseCLI = do
  yo <- cmdArgs yenteOptions
  _  <- validateOptions yo
  return yo
-- main = print =<< cmdArgs yenteOptions




data YenteOptions = YenteOptions
  { fromFile            :: String
  , toFile              :: String
  , phonetic_algorithm  :: Maybe String
  , retain_numeric      :: Bool
  , max_token_length    :: Maybe Int
  , misspelling_penalty :: Maybe Double

  , number_of_results   :: Int
  , include_ties        :: Bool
  , minimum_match_score :: Double
  , subgroup_search     :: Bool

  , output_file         :: Maybe String
  } deriving (Show, Data, Typeable)






yenteOptions = YenteOptions
  { fromFile           = def &= argPos 0 &= typ "FROM-FILE"
  , toFile             = def &= argPos 1 &= typ "TO-FILE"

  -- Preprocessing options
  , phonetic_algorithm = def   &= opt "phonetic-algorithm" &= help "Phoentic algorithm: Metaphone, Phonix, SoundEx (TO DO: Metaphone)" &= name "p"     &= groupname "Preprocessing" 
  , max_token_length   = def   &= opt "token" &= help "Trim each word to have a maximum number of letters. Can be useful for phonetic based matching." &= groupname "Preprocessing" 
  , retain_numeric     = False &= help "Retain numerical characters. Does not work with phonetic algorithms."                  &= groupname "Preprocessing"

  -- Matching options
  , misspelling_penalty = def &= opt "1" &= help "The misspelling penalty factor. (Empty to not allow misspellings, otherwise percent correct letters raised to factor serves as penalty)" &= groupname "Matching"

  -- Results options
  , number_of_results    = 1 
                        &= help "The number of results to output (Default 1)." &= groupname "Results"

  , include_ties         = False 
                        &= help "Include ties in output."  &= groupname "Results"

  , minimum_match_score  = 0.01 
                        &= opt "score"
                        &= help "The minimum score required to be considered a match. (Default 0.01) (TO DO: Fix, currently non-matches skipped)" &= groupname "Results"

  , subgroup_search      = False 
                        &= help "Search for matches in groups (requires 'group' column)."  
                        &= name "g"
                        &= groupname "Results"

  -- Output options
  , output_file          = def 
                        &= opt "output_file"
                        &= help "Copy results to an output file. (Default is a tab delimited file. Files with .csv extension will be comma delimited."  &= groupname "Output"
  } 
  &= program    "yente"
  &= summary "Yente - a matchmaker for text files (Version 0.2.1)"
  &= details ["More details at github.com/tumarkin/yente"] 
  &= helpArg    [explicit, name "help", groupname "Information"]
  &= versionArg [explicit, name "version", name "V", groupname "Information"]


validateOptions :: YenteOptions -> IO ()
validateOptions yo 
  = if   (isJust (phonetic_algorithm yo) && retain_numeric yo) 
    then error "Phonetic algorithms are not compatible with retaining numeric characters."
    else return ()
  









