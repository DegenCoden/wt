{-# LANGUAGE OverloadedStrings #-}

module Wt.UI.Select where

import Brick
import Brick.Widgets.List
import Data.Vector
import Graphics.Vty
import Wt.UI.Header

type Keybinds f a = (Key -> Maybe (a -> f a))

type DrawElement a = (a -> [Widget ()])

data SelectContext f a = SelectContext
  { selectDrawElement :: DrawElement a,
    selectHints :: String,
    selectItems :: [a],
    selectKeyBinds :: Keybinds f a,
    selectAttrs :: [(AttrName, Attr)]
  }

data SelectState f a
  = Select (List () a)
  | Done (f a)

drawUI :: String -> DrawElement a -> SelectState f a -> [Widget ()]
drawUI hints w (Select l) = [vBox [header hints, renderList (drawListElement w) True l]]
drawUI _ _ _ = undefined

theMap :: [(AttrName, Attr)] -> AttrMap
theMap attrs =
  attrMap
    defAttr
    ( attrs
        <> headerAttrs
        <> [ (listAttr, white `on` black),
             (listSelectedAttr, black `on` white)
           ]
    )

drawListElement :: DrawElement a -> Bool -> a -> Widget ()
drawListElement w b a =
  padAll 1 $
    withAttr (if b then "selected" else mempty) $
      hBox [hBox $ w a]

viEventHandler :: Event -> List () a -> EventM () (Next (SelectState f a))
viEventHandler e l = continue . Select =<< handleListEventVi handleListEvent e l

appEvent :: Keybinds f a -> SelectState f a -> BrickEvent () e -> EventM () (Next (SelectState f a))
appEvent kb (Select l) (VtyEvent e@(EvKey k _)) =
  maybe
    (viEventHandler e l)
    (halt . Done)
    (kb k <*> (snd <$> listSelectedElement l))
appEvent _ s _ = continue s

initialSelectState :: [a] -> SelectState f a
initialSelectState xs = Select $ list () (fromList xs) 1

select :: SelectContext f a -> IO (f a)
select ctx = do
  let app =
        App
          { appDraw = drawUI (selectHints ctx) (selectDrawElement ctx),
            appChooseCursor = showFirstCursor,
            appHandleEvent = appEvent $ selectKeyBinds ctx,
            appStartEvent = return,
            appAttrMap = const $ theMap $ selectAttrs ctx
          }

  x <- defaultMain app (initialSelectState $ selectItems ctx)

  case x of
    Done a -> return a
    Select _ -> select ctx
