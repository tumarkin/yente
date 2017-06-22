{-# LANGUAGE DeriveDataTypeable #-}

module App.Yente.CLICmdArgs
  ( YenteOptions(..)
  , parseCLI
  ) where

import           System.Console.CmdArgs.Implicit

import           App.Yente.Prelude


parseCLI :: IO YenteOptions
parseCLI = do
  yo <- cmdArgs yenteOptions
  validateOptions yo
  return yo


data YenteOptions = YenteOptions
  { fromFile           :: String
  , toFile             :: String
  , phoneticAlgorithm  :: Maybe String
  , retainNumeric      :: Bool
  , retainUnicode      :: Bool
  , maxTokenLength     :: Maybe Int
  , misspellingPenalty :: Maybe Double

  , numberOfResults    :: Int
  , includeTies        :: Bool
  , minimumMatchScore  :: Double
  , subgroupSearch     :: Bool

  , outputFile         :: Maybe String
  } deriving (Show, Data, Typeable)





yenteOptions = YenteOptions
  { fromFile           = def &= argPos 0 &= typ "FROM-FILE"
  , toFile             = def &= argPos 1 &= typ "TO-FILE"

  -- Preprocessing options
  , phoneticAlgorithm = def   &= opt "phonetic-algorithm" &= help "Phoentic algorithm: Metaphone, Phonix, SoundEx (TO DO: Metaphone)" &= name "p"     &= groupname "Preprocessing"
  , maxTokenLength   = def   &= opt "token" &= help "Trim each word to have a maximum number of letters. Can be useful for phonetic based matching." &= groupname "Preprocessing"
  , retainNumeric      = False &= help "Retain numerical characters. Does not work with phonetic algorithms."                  &= groupname "Preprocessing"
  , retainUnicode      = False &= help "Retain unicode characters. Does not work with phonetic algorithms."                  &= groupname "Preprocessing"

  -- Matching options
  , misspellingPenalty = def &= opt "1" &= help "The misspelling penalty factor. (Empty to not allow misspellings, otherwise percent correct letters raised to factor serves as penalty)" &= groupname "Matching"

  -- Results options
  , numberOfResults    = 1
                        &= help "The number of results to output (Default 1)." &= groupname "Results"

  , includeTies         = False
                        &= help "Include ties in output."  &= groupname "Results"

  , minimumMatchScore  = 0.01
                        &= opt "score"
                        &= help "The minimum score required to be considered a match. (Default 0.01) (TO DO: Fix, currently non-matches skipped)" &= groupname "Results"

  , subgroupSearch      = False
                        &= help "Search for matches in groups (requires 'group' column)."
                        &= name "g"
                        &= groupname "Results"

  -- Output options
  , outputFile          = def
                        &= opt "output_file"
                        &= help "Copy results to an output file. (Default is a tab delimited file. Files with .csv extension will be comma delimited."  &= groupname "Output"
  }
  &= program    "yente"
  &= summary "Yente - a matchmaker for text files (Version 0.3.0.2)"
  &= details ["More details at github.com/tumarkin/yente"]
  &= helpArg    [explicit, name "help", groupname "Information"]
  &= versionArg [explicit, name "version", name "V", groupname "Information"]


validateOptions :: YenteOptions -> IO ()
validateOptions YenteOptions{..}
  = when (isJust phoneticAlgorithm && retainNumeric) 
         (error "Phonetic algorithms are not compatible with retaining numeric characters.")










