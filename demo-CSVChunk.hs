{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

import CSVChunk (splitFileIO)

main :: IO ()
main = getArgs >>= \case
  [unit, n, inPath, outPrefix] -> 
    let withSize mult = splitFileIO (mult * read n) inPath outPrefix in
    case unit of 
      ('-':'-':'m':_) -> withSize $ 1024 * 1024
      ('-':'-':'k':_) -> withSize $ 1024
      ('-':'-':'b':_) -> withSize $ 1
      _ -> useage
  _ -> useage
  where
    useage = putStrLn $ "USEAGE \n"
      ++"  --megabytes inPath outPrefix\n"
      ++"  --kilobytes inPath outPrefix\n"
      ++"  --bytes inPath outPrefix"
