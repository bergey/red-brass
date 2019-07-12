{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SchoolDistricts where

import           ChartUtil

import           Reflex.Dom.Core

import           Control.Lens
import           Control.Monad
import           Data.Csv
import           Data.Foldable
import           Data.List (elemIndex, sortOn)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

data Expenditure = Expenditure
    { district :: T.Text
    , county :: T.Text
    , enrollment :: Double
    , total :: Double
    } deriving (Eq, Show)

instance FromNamedRecord Expenditure where
    parseNamedRecord r =
        Expenditure <$> r .: "district" <*> r .: "county" <*> r .: "adm" <*> r.: "total"

barWidthChart :: (DomBuilder t m, PostBuild t m) => Dynamic t (V.Vector Expenditure) -> m ()
barWidthChart d_ex = void $ dyn $  d_ex <&> \unsorted -> let
        exs = V.fromListN (V.length unsorted) $ sortOn total (toList unsorted) -- TODO mutable vector sort
        x_max = sum (enrollment <$> exs)
        xScale = xScale' x_max
        y_max = if V.null exs then 3e4 else maximum (total <$> exs)
        yScale = yScale' y_max
        cumulative = V.prescanl' (+) 0 (fmap enrollment exs)
        colors = V.generate (V.length exs) (\i -> if even i then "#77F" else "#99F")
        in svgAttr "svg" ( "height" =: showT outer_height <> "width" =: showT outer_width <> "style" =: "border: 1px solid cyan; display: block" ) $ do
            for_ (V.zip3 cumulative colors exs) $ \(x0, color, ex) -> let
                x1 = x0 + enrollment ex
                y1 = total ex
                in rectCorners (xScale x0, yScale 0) (xScale x1, yScale y1) ("fill" =: color)
            -- X axis
            line (margin_left - 1, outer_height - margin_bottom) (outer_width - margin_right, outer_height - margin_bottom) "black"
            when (x_max > 0) $ do
                let ticks = [ (2e5, "200k"), (4e5, "400k"), (6e5, "600k"), (8e5, "800k"), (1e6, "1M"), (1.2e6, "1.2M"), (1.4e6, "1.4M"), (1.6e6, "1.6M"), (1.8e6, "1.8M") ] & filter (\(x, _) -> x < x_max)
                for_ ticks $ \(x, label) -> do
                    line
                        (xScale x, outer_height - margin_bottom)
                        (xScale x, outer_height - margin_bottom + 5) "black"
                    textBottom (xScale x, outer_height - margin_bottom + 10) label
            textBottom (margin_left + (outer_width - margin_left - margin_right) / 2, outer_height - 30) "Number of Students"
            -- Y axis
            line (margin_left, outer_height - margin_bottom) (margin_left, margin_top) "black"
            let ticks = [5e3, 1e4 .. 3e4] & filter (< y_max)
            for_ ticks $ \y -> do
                line (margin_left, yScale y) (margin_left - 5, yScale y) "black"
                textLeft (margin_left -12, yScale y) (showT y)
            let y_axis_y = margin_top + (outer_height - margin_top - margin_bottom) / 2
            svgText (20, y_axis_y) "Expenditure Per Student [USD]" ( "transform" =: ("rotate(-90, 20, " <> showT y_axis_y <> ")")  <> "text-anchor" =: "middle" )





fetchCsv :: PrerenderClientConstraint js t m => m (Dynamic t (V.Vector Expenditure))
fetchCsv = do
    load <- getPostBuild
    let req = xhrRequest "GET" "https://raw.githubusercontent.com/bergey/school-districts/master/data/expenditures-2012-2013.csv" def
    response <- performRequestAsync (req <$ load)
    expenditures <- response & foldDynMaybe
        (\r _ -> let body = r ^. xhrResponse_responseText
            in case decodeByName . BSL.fromStrict . T.encodeUtf8 <$> body of
                Just (Right (_, e)) -> Just e
                _ -> Nothing
        ) mempty
    return expenditures

outer_width,outer_height,bar_width,margin_left,margin_bottom,margin_top,margin_right,padding_left,padding_right,padding_bottom,padding_top :: Double
outer_width = 800
outer_height = 500
bar_width = 19
margin_left = 100
margin_bottom = 70
margin_top = 50
margin_right = 30
padding_left = 0
padding_right = 0
padding_bottom = 0
padding_top = 0

xScale' :: Double -> Double -> Double
xScale' totalEnrollment = \enrollment ->  left + range * enrollment / totalEnrollment where
  left = margin_left + padding_left
  range = outer_width - margin_right - padding_right - left

yScale' :: Double -> Double -> Double
yScale' maxExpenditure = \ex -> range_min + range * ex / maxExpenditure where
  range_min = outer_height - margin_bottom - padding_bottom
  range_max = margin_top + padding_top
  range = range_max - range_min
