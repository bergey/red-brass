{-# LANGUAGE OverloadedStrings #-}
module ChartUtil where

import Reflex.Dom.Core

import qualified Data.Text as T
import qualified Data.Map as M

svgXMLNamespace :: T.Text
svgXMLNamespace = "http://www.w3.org/2000/svg"

elAttrNS' :: (DomBuilder t m, PostBuild t m) => T.Text -> T.Text -> M.Map T.Text T.Text -> m a -> m a
elAttrNS' ns tag attrs inner = snd <$> elDynAttrNS' (Just ns) tag (pure attrs) inner

svgAttr :: (DomBuilder t m, PostBuild t m) => T.Text -> M.Map T.Text T.Text -> m a -> m a
svgAttr = elAttrNS' svgXMLNamespace

svgClass :: (DomBuilder t m, PostBuild t m) => T.Text -> T.Text -> m a -> m a
svgClass tag c = svgAttr tag ( "class" =: c )

showT :: Show a => a -> T.Text
showT = T.pack . show

line :: (DomBuilder t m, PostBuild t m) => (Double, Double) -> (Double, Double) -> T.Text -> m ()
line (x1, y1) (x2, y2) stroke = svgAttr "line"
    ( "x1" =: showT x1 <> "y1" =: showT y1 <> "x2" =: showT x2 <> "y2" =: showT y2 <> "stroke" =: stroke )
    blank

rect :: (DomBuilder t m, PostBuild t m) => (Double, Double) -> Double -> Double -> M.Map T.Text T.Text -> m ()
rect (x, y) width height attrs = svgAttr "rect"
    ( "x" =: showT x <> "y" =: showT y <> "width" =: showT width <> "height" =: showT height  <> attrs)
    blank

-- | Take two corners, rather than width & height.  Checks min / max for each dimension.
rectCorners :: (DomBuilder t m, PostBuild t m) => (Double, Double) -> (Double, Double) -> M.Map T.Text T.Text -> m ()
rectCorners  (x0, y0) (x1, y1) attrs = rect (xMin, yMin) width height attrs where
  xMin = min x0 x1
  yMin = min y0 y1
  width = max x0 x1 - xMin
  height = max y0 y1 - yMin

svgText :: (DomBuilder t m, PostBuild t m) => (Double, Double) -> T.Text -> M.Map T.Text T.Text -> m ()
svgText (x, y) s attrs = svgAttr "text" ( "x" =: showT x <> "y" =: showT y <> attrs ) (text s)

textLeft :: (DomBuilder t m, PostBuild t m) => (Double, Double) -> T.Text -> m ()
textLeft (x, y) s = svgText (x, y) s
    ( "text-anchor" =: "end" <> "alignment-baseline" =: "middle" )

textBottom :: (DomBuilder t m, PostBuild t m) => (Double, Double) -> T.Text -> m ()
textBottom (x, y) s = svgText (x, y) s
    ( "text-anchor" =: "middle" <> "alignment-baseline" =: "hanging" )

circle :: (DomBuilder t m, PostBuild t m) => (Double, Double) -> Double -> M.Map T.Text T.Text -> m ()
circle (x, y) r attrs = svgAttr "circle"
    ( "cx" =: showT x <> "cy" =: showT y <> "r" =: showT r <> attrs )
    blank

linspace :: (Double, Double) -> Int -> [Double]
linspace (lower, upper) count
    | count < 2 = error "linspace: count must be greater than 2"
    | upper <= lower = error "linspace: upper must be greater than lower"
    | otherwise = [lower, next .. upper] where
        next = lower + (upper - lower) / fromIntegral (count - 1)
