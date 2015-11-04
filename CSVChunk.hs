{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module CSVChunk where

import qualified Control.Monad.Parallel as MP
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

import Control.Exception.Base (bracket)
import Control.Monad (liftM2, foldM)
import Control.Monad.State.Lazy (runState, get, put, State)
import Data.ByteString.Builder (Builder, hPutBuilder, char8, lazyByteString)
import Data.Int (Int64)
import Data.Monoid ((<>))
import System.Directory (removeFile)
import System.IO (IOMode(WriteMode), withFile)

import Prelude hiding (head)

errSelf :: String -> a
errSelf = error . ("CSVChunk."++)

type BC = BC.ByteString
type BLC = BLC.ByteString

--I'm gonna see how well this monad-parallel library works.
foldMapSplitFile :: (Monoid m) 
  => Int64 -> FilePath -> (BC -> BC -> m) -> IO m
foldMapSplitFile chunkSize inPath toMonoid = withSplit chunkSize inPath
  $ uncurry $ \ hdr -> fmap mconcat . MP.mapM
    (fmap (foldMap (toMonoid hdr) . BC.lines) . BC.readFile)

withSplit :: Int64 -> FilePath -> ((BC, [FilePath]) -> IO b) -> IO b
withSplit chunkSize inPath 
  = bracket (splitFileIO chunkSize inPath) (uncurry $ \ _ -> mapM_ removeFile )

splitFileIO :: Int64 -> FilePath -> IO (BC, [FilePath])
splitFileIO chunkSize inPath = (uncurry $ \ hdr
    ->fmap (\ (_, paths) -> (BLC.toStrict $ BLC.copy hdr, reverse paths) )
    . foldM (\ (fileNum, paths) ->
        let outPath = inPath++".chunk."++show fileNum in
        fmap (const (fileNum + 1, outPath:paths)) . writeFileBldr outPath
      ) (0 :: Int, [])
    . splitFile chunkSize
  ) . BLC.break (=='\n') =<< BLC.readFile inPath

splitFile :: Int64 -> BLC.ByteString -> [Builder]
splitFile chunkSize = fst . runState go . BLC.lines
  where
    go = get >>= \case
      [] -> return []
      _ -> liftM2 (:) (mkChunk chunkSize) go
    mkChunk :: Int64 -> State [BLC.ByteString] Builder
    mkChunk chunkRemaining = get >>= \case
      (row:rows) -> if chunkRemaining > 0
        then put rows 
          >> ((lazyByteString row<>char8 '\n')<>)
          <$>(mkChunk $ chunkRemaining - BLC.length row - 1)
        else end
      [] -> end
      where end = return mempty --just in case I have to change this back to \n

writeFileBldr :: FilePath -> Builder -> IO ()
writeFileBldr outFile bldr = withFile outFile WriteMode $ flip hPutBuilder bldr
