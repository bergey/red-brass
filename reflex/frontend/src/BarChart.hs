{-# LANGUAGE OverloadedStrings #-}
module BarChart where

import ChartUtil

import Reflex.Dom.Core

import Data.Foldable
import Data.List (elemIndex)

import qualified Data.Text as T

readings :: [(T.Text, Double)]
readings = zip (map T.singleton ['A'..]) [81, 41, 75, 56, 58, 3, 41, 92, 58, 27]

barChart :: (DomBuilder t m, PostBuild t m) => m ()
barChart = elAttr "svg"
  ( "height" =: "500" <> "width" =: "500" <> "style" =: "border: 1px solid cyan; display: block" ) $ do
    for_ readings $ \(i, d) -> rect (x i - bar_width / 2, y d) bar_width (y 0 - y d) ( "fill" =: "#666" )
    -- x-axis
    line (margin_left - 1, outer_height - margin_bottom) (outer_width - margin_right, outer_height - margin_bottom) "black"
    for_ readings $ \(i, _d) -> line (x i, outer_height - margin_bottom) (x i, outer_height - margin_bottom + 5) "black"
    for_ (map fst readings) $ \ label ->
        textBottom (x label, outer_height - margin_bottom + 10) label
    -- y-axis
    line (margin_left, outer_height - margin_bottom + 1) (margin_left, margin_top) "black"
    for_ [0, 10..100] $ \d -> line (margin_left, y d) (margin_left - 5, y d) "black"
    for_ [30, 60, 90] $ \d -> textLeft (margin_left - 10, y d) (showT d)

outer_width,outer_height,bar_width,margin_left,margin_bottom,margin_top,margin_right,padding_left,padding_right,padding_bottom,padding_top :: Double
outer_width = 500
outer_height = 500
bar_width = 19
margin_left = 50
margin_bottom = 50
margin_top = 50
margin_right = 50
padding_left = bar_width * 1.5
padding_right = padding_left
padding_bottom = 0
padding_top = 0

-- scaling functions, like D3 (although harder to inspect)
x :: T.Text -> Double
x label = range_min + m * fromIntegral i where
    knownLabels = map fst readings
    i = case elemIndex label knownLabels of
        Just i -> i
        Nothing -> error ( "tried to compute x-position of unknown label: " <> T.unpack label <> "\n    known labels are: " <> show knownLabels )
    range_min = margin_left + padding_left
    range_max = outer_width - margin_right - padding_right
    domain_width = length readings - 1
    m = (range_max - range_min) / fromIntegral domain_width

y :: Double -> Double
y d = range_min + m * d where
    range_min = outer_height - margin_bottom - padding_bottom
    range_max = margin_top + padding_top
    domain_width = 100
    m = (range_max - range_min) / domain_width
