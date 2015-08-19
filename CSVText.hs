{-# OPTIONS_GHC -Wall #-} 
{-# LANGUAGE OverloadedStrings #-}
module CSVText where

import qualified Data.ByteString as B
import qualified Data.Text as T

import Data.Text.Encoding (decodeLatin1)

readLatin1 :: FilePath -> IO T.Text
readLatin1 = fmap decodeLatin1 . B.readFile

csvs, tsvs :: T.Text -> ([T.Text], [[T.Text]])
csvs = xsvs ','  retOrNewLines
tsvs = xsvs '\t' retOrNewLines

xsvs :: Char -> (T.Text -> [T.Text]) -> T.Text -> ([T.Text], [[T.Text]])
xsvs separator splitLines text
  = case fmap (T.split (==separator)) $ splitLines text of
    [] -> ([], [])
    (header:rowsOfColumns) -> (header, rowsOfColumns)

dosLines :: T.Text -> [T.Text]
dosLines = T.splitOn "\r\n"

retOrNewLines :: T.Text -> [T.Text]
retOrNewLines = T.split (\ c -> c=='\n' || c=='\r')
