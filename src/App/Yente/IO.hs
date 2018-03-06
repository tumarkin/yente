{-# LANGUAGE FlexibleInstances #-}

module App.Yente.IO
    ( -- NameComparison(..)
      readNamesFile
    -- , getFileFormat
    , getEncodeOptions
    -- , SupportedFileFormat(..)
    ) where


import qualified Data.ByteString.Lazy as BS
import           Data.Char            (ord)
import           Data.Csv             hiding (lookup)
import           System.FilePath

import           App.Yente.Prelude
import           App.Yente.Types


-- | Get the encoding options for a given filetype
getEncodeOptions ∷ FilePath → EncodeOptions
getEncodeOptions fp = 
  case getFileFormat fp of
    CommaDelimited → csvEncoding
    TabDelimited   → tabEncoding

-- | Get file format from file name
getFileFormat ∷ FilePath → SupportedFileFormat
getFileFormat fn = case takeExtension fn of
                    ".csv" -> CommaDelimited
                    _     -> TabDelimited

data SupportedFileFormat
    = CommaDelimited
    | TabDelimited
    deriving (Show, Eq)



-- Encoding and decoding formats
csvEncoding = defaultEncodeOptions
tabEncoding = defaultEncodeOptions { encDelimiter = fromIntegral (ord '\t') }

csvDecoding = defaultDecodeOptions
tabDecoding = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }


-- File reading functions
readNamesFile ∷ MonadIO m ⇒ FilePath
              → m (Vector NameRaw)
readNamesFile fp = do
    bstring         <- liftIO $ BS.readFile fp
    let decodedFile = decodeByNameWith decOpts bstring
    return (snd . fromEither $ decodedFile)
  where
    fromEither a = case a of
        Left e  -> error e
        Right x -> x
    decOpts = getDecodeOptions fp


getDecodeOptions ∷ FilePath → DecodeOptions
getDecodeOptions fp =
  case getFileFormat fp of
    CommaDelimited → csvDecoding
    TabDelimited   → tabDecoding


instance FromNamedRecord NameRaw where
    parseNamedRecord m = emptyName
                      <$> m .:   "id"
                      <*> m .:   "name"
                      <*> m `lookupOptionalCol`  "group"

lookupOptionalCol ∷ NamedRecord → ByteString → Parser Text
lookupOptionalCol n bs =
  case lookup bs n of
    Nothing -> pure ""
    Just s  -> pure . cs $ s



