{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text hiding (reverse)
import Options.Applicative
import Wt.Data.Torrent
import Wt.Network.HTTP.TorrentSearch
import Wt.Operations
import Wt.UI.SelectTorrent

data Command = Search Text | WatchLink MagnetLink

query :: Parser Command
query = Search <$> argument str (metavar "QUERY" <> help "Search query sent to the search engine")

watchLink :: Parser Command
watchLink = WatchLink <$> argument str (metavar "MAGNETLINK" <> help "Magnet link to watch")

commands :: Mod CommandFields Command
commands =
  mconcat
    [ command "search" (info query (progDesc "Search torrent to watch")),
      command "watch" (info watchLink (progDesc "Watch using magnet link"))
    ]

parser :: Parser Command
parser = subparser commands <|> query

desc :: InfoMod a
desc =
  fullDesc
    <> header "WT - Watch Torrents"
    <> progDesc "Utility for searching for, downloading, and streaming torrents"

opts :: ParserInfo Command
opts = info (parser <**> helper) desc

trackers :: [Tracker]
trackers =
  [ "http://nyaa.tracker.wf:7777/announce",
    "udp://exodus.desync.com:6969/announce",
    "udp://open.stealth.si:80/announce",
    "udp://tracker.coppersurfer.tk:6969/announce",
    "udp://tracker.internetwarriors.net:1337",
    "udp://tracker.opentrackr.org:1337/announce"
  ]

main :: IO ()
main = do
  o <- execParser opts
  case o of
    WatchLink ml -> watch ml
    Search q -> do
      s <- selectTorrent =<< search q
      case s of
        Watch t -> watch $ magnetLink trackers t
        Print t -> print $ magnetLink trackers t
        Download t -> download $ magnetLink trackers t
        Exit -> return ()
