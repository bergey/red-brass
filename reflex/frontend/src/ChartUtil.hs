{-# LANGUAGE OverloadedStrings #-}
module ChartUtil where

import Reflex.Dom.Core

import qualified Data.Text as T
import qualified Data.Map as M

showT :: Show a => a -> T.Text
showT = T.pack . show

line :: DomBuilder t m => (Double, Double) -> (Double, Double) -> T.Text -> m ()
line (x1, y1) (x2, y2) stroke = elAttr "line"
    ( "x1" =: showT x1 <> "y1" =: showT y1 <> "x2" =: showT x2 <> "y2" =: showT y2 <> "stroke" =: stroke )
    blank

rect :: DomBuilder t m => (Double, Double) -> Double -> Double -> M.Map T.Text T.Text -> m ()
rect (x, y) width height attrs = elAttr "rect"
    ( "x" =: showT x <> "y" =: showT y <> "width" =: showT width <> "height" =: showT height  <> attrs)
    blank

textLeft :: DomBuilder t m => (Double, Double) -> T.Text -> m ()
textLeft (x, y) s = elAttr "text"
    ( "x" =: showT x <> "y" =: showT y <> "text-anchor" =: "end" <> "alignment-baseline" =: "middle" )
    (text s)

textBottom :: DomBuilder t m => (Double, Double) -> T.Text -> m ()
textBottom (x, y) s = elAttr "text"
    ( "x" =: showT x <> "y" =: showT y <> "text-anchor" =: "middle" <> "alignment-baseline" =: "hanging" )
    (text s)

circle :: DomBuilder t m => (Double, Double) -> Double -> M.Map T.Text T.Text -> m ()
circle (x, y) r attrs = elAttr "circle"
    ( "cx" =: showT x <> "cy" =: showT y <> "r" =: showT r <> attrs )
    blank