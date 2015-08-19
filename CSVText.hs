{-# OPTIONS_GHC -Wall #-} 
module CSVText where

import qualified Data.ByteString as B
import qualified Data.Text as T

import Data.Text.Encoding (decodeLatin1)

readLatin1 :: FilePath -> IO T.Text
readLatin1 = fmap decodeLatin1 . B.readFile

type HeaderRowsofColumns = ([T.Text], [[T.Text]])

csvs, tsvs :: T.Text -> HeaderRowsofColumns
csvs = xsvs ','; tsvs = xsvs '\t'

xsvs :: Char -> T.Text -> HeaderRowsofColumns
xsvs separator text 
  = case fmap (T.split (==separator)) $ retLines text of
    [] -> ([], [])
    (header:rowsOfColumns) -> (header, rowsOfColumns)

retLines :: T.Text -> [T.Text]
retLines = T.split (\ c -> c=='\n' || c=='\r')
