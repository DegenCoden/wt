{-# LANGUAGE OverloadedStrings #-}

module Wt.UI.SelectTorrent where

import Brick
import Brick.Widgets.List
import Graphics.Vty
import Wt.Data.Torrent
import Wt.UI.Select

data Action a
  = Watch a
  | Download a
  | Print a
  | Exit

attrs :: [(AttrName, Attr)]
attrs =
  [ ("engine" <> listSelectedAttr, defAttr `withForeColor` black),
    ("engine", defAttr `withForeColor` yellow),
    ("leech" <> listSelectedAttr, defAttr `withForeColor` black),
    ("leech", defAttr `withForeColor` red),
    ("seed" <> listSelectedAttr, defAttr `withForeColor` black),
    ("seed", defAttr `withForeColor` brightGreen)
  ]

drawElement :: Torrent -> [Widget ()]
drawElement t =
  [ padRight Max $ txtWrap $ title t,
    hBox
      ( padLeft (Pad 1)
          <$> [ str $ show (div (size t) 1000000) <> " Mb",
                left "seed" $ seed t,
                left "leech" $ leech t
              ]
      )
  ]
  where
    left name =
      withAttr name
        . hLimit 4
        . padLeft Max
        . str
        . show

keybinds :: Keybinds Action Torrent
keybinds c = case c of
  KChar 'd' -> Just Download
  KChar 'p' -> Just Print
  KChar 'q' -> Just $ const Exit
  KEnter -> Just Watch
  _ -> Nothing

selectTorrent :: [Torrent] -> IO (Action Torrent)
selectTorrent [] = putStrLn "No results" >> return Exit
selectTorrent items =
  select
    SelectContext
      { selectAttrs = attrs,
        selectDrawElement = drawElement,
        selectHints = "[d]ownload [p]rint [q]uit",
        selectItems = items,
        selectKeyBinds = keybinds
      }
