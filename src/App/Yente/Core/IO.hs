{-# LANGUAGE FlexibleInstances #-}

module App.Yente.Core.IO
  ( readNamesFile
  , getEncodeOptions
  ) where


import           ClassyPrelude
import qualified Data.ByteString.Lazy    as BS
import           Data.Char               (ord)
import           Data.Csv                hiding (Name, lookup)
import           Data.String.Conversions (cs)
import           System.FilePath         (takeExtension)

-- -- import           App.Yente.Prelude
import           App.Yente.Core.Types



--------------------------------------------------------------------------------
-- Types                                                                      --
--------------------------------------------------------------------------------
data SupportedFileFormat
    = CommaDelimited
    | TabDelimited
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Encoding                                                                   --
--------------------------------------------------------------------------------
-- | Get the encoding options for a given filetype
getEncodeOptions ∷ FilePath → EncodeOptions
getEncodeOptions fp =
  case getFileFormat fp of
    CommaDelimited → csvEncoding
    TabDelimited   → tabEncoding

csvEncoding = defaultEncodeOptions
tabEncoding = defaultEncodeOptions { encDelimiter = fromIntegral (ord '\t') }

--------------------------------------------------------------------------------
-- Decoding                                                                   --
--------------------------------------------------------------------------------
getDecodeOptions ∷ FilePath → DecodeOptions
getDecodeOptions fp =
  case getFileFormat fp of
    CommaDelimited → csvDecoding
    TabDelimited   → tabDecoding


csvDecoding = defaultDecodeOptions
tabDecoding = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }

--------------------------------------------------------------------------------
-- Utility functions                                                          --
--------------------------------------------------------------------------------
-- | Get file format from file name
getFileFormat ∷ FilePath → SupportedFileFormat
getFileFormat fn = case takeExtension fn of
                    ".csv" -> CommaDelimited
                    _      -> TabDelimited



 --------------------------------------------------------------------------------
 --IO                                                                          --
 --------------------------------------------------------------------------------
readNamesFile ∷ MonadIO m ⇒ FilePath
              → m (Vector (Name ()))
readNamesFile fp = do
    bstring         <- liftIO $ BS.readFile fp
    let decodedFile = decodeByNameWith decOpts bstring
    return (snd . fromEither $ decodedFile)
  where
    fromEither a = case a of
        Left e  -> error e
        Right x -> x
    decOpts = getDecodeOptions fp


instance FromNamedRecord (Name ()) where
    parseNamedRecord m = Name
                      <$> m .:   "id"
                      <*> m .:   "name"
                      <*> m `lookupOptionalCol`  "group"
                      <*> pure ()

lookupOptionalCol ∷ NamedRecord → ByteString → Parser Text
lookupOptionalCol n bs =
  case lookup bs n of
    Nothing -> pure ""
    Just s  -> pure . cs $ s



