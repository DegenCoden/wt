{-# LANGUAGE OverloadedStrings #-}

module Wt.Data.Torrent where

import Data.Text
import GHC.Exts (IsString (..))
import Prelude hiding (unwords)

newtype Tracker = Tracker {unTracker :: Text}

instance IsString Tracker where
  fromString = Tracker . pack

newtype MagnetLink = MagnetLink {unMagnetLink :: Text}

instance Show MagnetLink where
  show = unpack . unMagnetLink

instance IsString MagnetLink where
  fromString = MagnetLink . pack

data Torrent = Torrent
  { leech :: Int,
    seed :: Int,
    size :: Int,
    title :: Text,
    urn :: Text
  }

magnetLink :: [Tracker] -> Torrent -> MagnetLink
magnetLink ts t =
  MagnetLink $
    "magnet:?xt=urn:btih:"
      <> urn t
      <> unwords (("&tr=" <>) . unTracker <$> ts)
