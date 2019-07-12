{-# LANGUAGE OverloadedStrings #-}
module Scatter where

import ChartUtil

import Reflex.Dom.Core

import Control.Monad
import Data.Foldable

readings :: [(Double, Double)]
readings = [ (81, 98),  (41, 13),  (75, 38),  (56, 28),  (58, 30),  ( 3, 75),  (41, 45),  (92, 99),  (58, 0 ),  (27, 69) ]

scatterPlot :: DomBuilder t m => m ()
scatterPlot = elAttr "svg"
  ( "height" =: "500" <> "width" =: "500" <> "style" =: "border: 1px solid cyan; display: block" ) $ do
    for_ readings $ \(x, y) -> circle (xScale x, yScale y) 3 mempty
    -- x-axis
    elAttr "g" ( "transform" =: ("translate(0, " <> showT (outer_height - margin_bottom) <> ")" )) $ do
      line (margin_left, 0) (outer_width - margin_right, 0) "black"
      for_ [0, 10 .. 100] $ \x -> do
        let bigTick = floor x `mod` 20 == 0
        line (xScale x, 0) (xScale x, if bigTick then 7.5 else 5) "black"
        when bigTick $ textBottom (xScale x, 15) (showT x)
      textBottom (xScale 50, 40) "Aardvarks"
    -- y-axis
    elAttr "g" ( "transform" =: ("translate(" <> showT margin_left <> ", 0)") ) $ do
      line (0, outer_height - margin_bottom) (0, margin_top) "black"
      for_ [0, 10 .. 100] $ \y -> do
        let bigTick = floor y `mod` 20 == 0
        line (0, yScale y) (if bigTick then -7.5 else -5, yScale y) "black"
        when bigTick $ textLeft (-12, yScale y) (showT y)
      svgText (-50, yScale 50) "Badgers" ( "transform" =: ("rotate(-90, -50, " <> showT (yScale 50) <> ")")  <> "text-anchor" =: "middle" )

outer_width, outer_height, margin_left, margin_bottom, margin_top, margin_right, padding_left, padding_right, padding_bottom, padding_top :: Double
outer_width = 500
outer_height = 500
margin_left = 70
margin_bottom = 70
margin_top = 30
margin_right = 30
padding_left = 10
padding_right = 0
padding_bottom = 10
padding_top = 0

xScale :: Double -> Double
xScale d = range_min + m * d where
    range_min = margin_left + padding_left
    range_max = outer_width - margin_right - padding_right
    domain_width = 100
    m = (range_max - range_min) / domain_width

yScale :: Double -> Double
yScale d = range_min + m * d where
    range_min = outer_height - margin_bottom - padding_bottom
    range_max = margin_top + padding_top
    domain_width = 100
    m = (range_max - range_min) / domain_width
