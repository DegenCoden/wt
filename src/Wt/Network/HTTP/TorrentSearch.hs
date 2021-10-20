{-# LANGUAGE OverloadedStrings #-}

module Wt.Network.HTTP.TorrentSearch where

import Data.Aeson
import Data.Aeson.Types
import Data.Function
import Data.List
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Wt.Data.Torrent

parseTorrent :: Value -> Parser Torrent
parseTorrent =
  withObject "Torrent" $ \v ->
    Torrent
      <$> v .: "l"
      <*> v .: "s"
      <*> v .: "len"
      <*> v .: "text"
      <*> v .: "id"

search :: Text -> IO [Torrent]
search q = do
  let req = setQueryString [("q", Just $ encodeUtf8 q)] "https://torrent-paradise.ml/api/search"
  mv <- decode . responseBody <$> (httpLbs req =<< newManager tlsManagerSettings)
  return
    . sortBy (flip compare `on` seed)
    . fromMaybe []
    $ traverse (parseMaybe parseTorrent) =<< mv
