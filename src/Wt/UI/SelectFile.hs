{-# LANGUAGE OverloadedStrings #-}

module Wt.UI.SelectFile where

import Brick
import Data.Text hiding (last)
import Graphics.Vty
import Wt.UI.Select
import Prelude hiding (unwords)

data Action a
  = Watch a
  | Exit

attrs :: [(AttrName, Attr)]
attrs = []

drawElement :: FilePath -> [Widget ()]
drawElement fp = [padRight Max $ txtWrap $ last (splitOn "/" $ pack fp)]

keybinds :: Keybinds Action FilePath
keybinds c = case c of
  KChar 'q' -> Just $ const Exit
  KEnter -> Just Watch
  _ -> Nothing

selectFile :: [FilePath] -> IO (Action FilePath)
selectFile items = do
  select
    SelectContext
      { selectAttrs = attrs,
        selectDrawElement = drawElement,
        selectHints = "[q]uit",
        selectItems = items,
        selectKeyBinds = keybinds
      }
