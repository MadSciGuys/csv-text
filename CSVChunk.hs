{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module CSVChunk where

import qualified Data.ByteString.Lazy.Char8 as BLC

import Control.Monad (liftM2, foldM_)
import Control.Monad.State.Lazy (runState, get, put, State)
import Data.ByteString.Builder (Builder, hPutBuilder, char8, lazyByteString)
import Data.Int (Int64)
import Data.Monoid ((<>))
import System.IO (IOMode(WriteMode), withFile)

splitFileIO :: Int64 -> FilePath -> FilePath -> IO ()
splitFileIO chunkSize inPath outPrefix = foldM_ (\ fileNum
    -> fmap (const $ fileNum + 1) 
     . writeFileBldr (outPrefix++".chunk."++show fileNum)
  ) (0 :: Int) . splitFile chunkSize =<< BLC.readFile inPath

splitFile :: Int64 -> BLC.ByteString -> [Builder]
splitFile chunkSize = fst . runState go . BLC.lines
  where
    go = get >>= \case
      [] -> return []
      _ -> liftM2 (:) (mkChunk chunkSize) go
    mkChunk :: Int64 -> State [BLC.ByteString] Builder
    mkChunk chunkRemaining = get >>= \case
      (row:rows) -> if chunkRemaining > 0
        then do
          put rows
          ((lazyByteString row<>char8 '\n')<>)
            <$>(mkChunk $ chunkRemaining - BLC.length row - 1)
        else end
      [] -> end
      where end = return mempty --just in case I have to change this back to \n

writeFileBldr :: FilePath -> Builder -> IO ()
writeFileBldr outFile bldr 
  = withFile outFile WriteMode $ flip hPutBuilder bldr
