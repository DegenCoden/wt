{-# LANGUAGE OverloadedStrings #-}

module Wt.Operations where

import Control.Exception
import Control.Monad.Fix
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.IO.Temp
import System.Process.Typed
import Wt.Data.Torrent
import Wt.UI.SelectFile

splitfps :: BS.ByteString -> [FilePath]
splitfps = lines . BS.unpack

ls :: FilePath -> IO [FilePath]
ls fp = splitfps . fst <$> readProcess_ (proc "find" [fp, "-type", "f"])

mpv :: FilePath -> IO ()
mpv fp = runProcess_ $ proc "mpv" [fp]

btfs :: MagnetLink -> FilePath -> FilePath -> ProcessConfig () () ()
btfs ml dl mnt = proc "btfs" ["--data-directory=" <> dl, show ml, mnt]

-- monad-loops
iterateWhile :: Monad m => (a -> Bool) -> m a -> m a
iterateWhile p m = fix (\rec -> m >>= \x -> if p x then rec else return x)

watch :: MagnetLink -> IO ()
watch ml = withSystemTempDirectory "wt-btfs" $ \fp -> do
  bracket
    ((,) <$> createTempDirectory fp "mnt" <*> createTempDirectory fp "dl")
    ( \(mnt, dl) -> do
        runProcess_ $ proc "fusermount" ["-u", mnt]
        removeDirectoryRecursive mnt
        removeDirectoryRecursive dl
    )
    ( \(mnt, dl) -> do
        withProcessWait (btfs ml dl mnt) $ \_ -> do
          sel <- selectFile =<< iterateWhile null (ls mnt)
          case sel of
            Watch f -> mpv f
            _ -> return ()
    )

download :: MagnetLink -> IO ()
download ml = withProcessWait (proc "aria2c" [show ml]) print
