{-# LANGUAGE OverloadedStrings #-}

module Wt.UI.Header where

import Brick
import Graphics.Vty

headerAttrs :: [(AttrName, Attr)]
headerAttrs = [("header", black `on` cyan), ("hints", yellow `on` black)]

header :: String -> Widget n
header hints =
  withAttr "header" $
    padAll 1 $ hBox [padRight Max $ str hints, str "█▁█▁█ ▔█▔"]
