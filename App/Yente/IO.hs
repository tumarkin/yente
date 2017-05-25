{-# LANGUAGE OverloadedStrings #-}

module App.Yente.IO
    ( NameComparison(..)
    , handleToNameWriter
    , filepathToNameWriter
    {-, handleToNameReader-}
    , filepathToNames
    , getFileFormat
    , SupportedFileFormat(..)
    , defaultFileFormat
    ) where


import           Control.Applicative
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BS
import           Data.Char             (ord)
import           Data.Csv              ((.:), (.=), FromField, NamedRecord, Parser)
import qualified Data.Csv              as CSV
-- import qualified Data.Map.Strict       as DM
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           System.IO
import qualified System.IO.Streams     as Streams
import           System.IO.Streams.Csv

import qualified Data.HashMap.Strict   as HM

import           App.Yente.Types
import App.Yente.Prelude








data SupportedFileFormat
    = CommaDelimited
    | TabDelimited
    deriving (Show, Eq)

instance CSV.FromNamedRecord Name where
    parseNamedRecord m = emptyName
                      <$> m .:   "id"
                      <*> m .:   "name"
                      <*> m `lookupOptionalCol`  "group"



instance CSV.ToNamedRecord NameComparison where
    toNamedRecord (NameComparison fn tn s )
        = CSV.namedRecord [ "score"     .= s
                          , "name_from" .= name fn
                          , "name_to"   .= name tn
                          , "id_from"   .= idx fn
                          , "id_to"     .= idx tn
                          ]



lookupOptionalCol :: NamedRecord -> B.ByteString -> Parser (Maybe Text)
lookupOptionalCol n bs =
  case HM.lookup bs n of
    Nothing -> pure Nothing
    Just s  -> pure . Just . T.pack . BSC.unpack $ s










-- Default file format
defaultFileFormat = TabDelimited

-- Get file format from file name
getFileFormat :: String -> SupportedFileFormat
getFileFormat fn = case extension of
                    "csv" -> CommaDelimited
                    "txt" -> TabDelimited
                    "tsv" -> TabDelimited
                    _     -> TabDelimited
  where
    extension = T.unpack . T.toLower . snd $ T.breakOnEnd (T.pack ".") (T.pack fn)






-- Writing functions

handleToNameWriter :: SupportedFileFormat
                   -> Handle
                   -> IO (Streams.OutputStream NameComparison)
handleToNameWriter ff h
    =   Streams.handleToOutputStream h
    >>= encodeStreamByNameWith (encoding ff) (V.fromList [ "score", "name_from", "name_to", "id_from", "id_to" ])

  where
    encoding TabDelimited   = tabEncoding
    encoding CommaDelimited = csvEncoding

filepathToNameWriter :: SupportedFileFormat
                   -> String
                   -> IO (Streams.OutputStream NameComparison)
filepathToNameWriter ff outFileName
    = openFile outFileName WriteMode >>= handleToNameWriter ff


-- Reading functions
filepathToNames :: String
                 -> IO [ Name ]
filepathToNames inFileName
    = do
    bstring         <- BS.readFile inFileName
    let decodedFile  = CSV.decodeByNameWith (decoding fileformat) bstring
    return (V.toList . snd . fromEither $ decodedFile)
  where
    decoding TabDelimited   = tabDecoding
    decoding CommaDelimited = csvDecoding
    fromEither a = case a of
        Left e  -> error e
        Right x -> x
    fileformat = getFileFormat inFileName

{-handleToNameReader :: SupportedFileFormat -}
{-                      -> Handle -}
{-                      -> IO (Streams.InputStream (Either  String Name))-}
{-handleToNameReader ff h -}
{-    = Streams.handleToInputStream h >>= decodeStreamByNameWith -}
{-                                        (decoding ff) -}
{-  where-}
{-    decoding TabDelimited   = tabDecoding-}
{-    decoding CommaDelimited = csvDecoding-}




-- Encoding and decoding formats
csvEncoding = CSV.defaultEncodeOptions
tabEncoding = CSV.defaultEncodeOptions { CSV.encDelimiter = fromIntegral (ord '\t') }

csvDecoding = CSV.defaultDecodeOptions
tabDecoding = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord '\t') }


