module App.Yente.CLIOptparseApplicative
  ( parseCLI
  ) where

import           Options.Applicative
import Data.Monoid ((<>))

import           App.Yente.Prelude
import           App.Yente.Types hiding (group)
import           System.Console.Terminal.Size


parseCLI :: IO YenteOptions
parseCLI = do
  ypp <- yParserPrefs . maybe 80 width <$> size
  
  validateOptions =<< customExecParser ypp yenteInfo



yParserPrefs :: Int -> ParserPrefs
yParserPrefs cols = prefs (showHelpOnError <> columns cols)



yenteInfo :: ParserInfo YenteOptions
yenteInfo = info (yenteOptions <**> helper)
  (fullDesc 
  <> header "Yente - a matchmaker for text files (Version 0.3.0.3)"
  <> footer "More details and Wiki at github.com/tumarkin/yente"
  )


yenteOptions :: Parser YenteOptions
yenteOptions = YenteOptions 
  <$> strArgument (metavar "FROM-FILE")
  <*> strArgument (metavar "TO-FILE")

  -- Preprocessing configuration
  <*> optional phoneticAlgorithmP
  <*> switch (long "retain-numerical" <> help "Retain numerical characters (does not work with phonetic algorithms)")
  <*> switch (long "retain-unicode"   <> help "Retain unicode characters (does not work with phonetic algorithms)")
  <*> optional (option auto ( long "token-length" 
                  <> short 't' 
                  <> metavar "INT"
                  <> help "Trim each word to have a maximum number of letters (can be useful f with phonetic-based matching)")
                  )

  -- Search configuration
  <*> optional (option auto ( long "misspelling-penalty" 
                  <> short 'p'
                  <> metavar "INT"
                  <> help "The misspelling penalty factor (percent correct letters raised to factor is multiplied by token score)"))
  <*> switch (long "subgroup-search" <> short 'g' <> help "Search for matches in groups (requires 'group' column in data files)")

  -- Output configuration
  <*> option auto ( long "number-of-results"
                  <> short 'n'
                  <> help "The number of results to output"
                  <> metavar "INT"
                  <> value 1
                  <> showDefault
                  )
  <*> switch (long "include-ties" <> short 'T' <> help "Include ties in output")
  <*> option auto ( long "minimum-match-score"
                  <> short 'm'
                  <> metavar "FLOAT"
                  <> help "The minimum score required to be considered a match"
                  <> value 0.01
                  <> showDefault
                  )
  <*> optional (strOption (long "output-file"
                        <> short 'o'
                        <> metavar "FILEPATH"
                        <> help "Copy results to an output file"
                        ))


phoneticAlgorithmP :: Parser PhoneticAlgorithm
phoneticAlgorithmP 
   =  flag' Phonix (long "phonix" 
                   <> help "Preprocess words with Phonix algorithm.")
  <|> flag' Soundex (long "soundex" 
                   <> help "Preprocess words with Soundex algorithm.") 

validateOptions :: YenteOptions -> IO YenteOptions
validateOptions yo@YenteOptions{..} = do 
  when (isJust phoneticAlgorithm && retainNumeric) 
         (error "Phonetic algorithms are not compatible with retaining numeric characters.")
  return yo










