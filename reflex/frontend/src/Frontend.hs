{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend where

import BarChart (barChart)
import ChartUtil
import Scatter (scatterPlot)
import SchoolDistricts (barWidthChart, fetchCsv)

import Reflex.Dom.Core

import Control.Monad
import Data.Maybe (fromMaybe)
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List (elemIndex, sortOn)

head :: (DomBuilder t m, PostBuild t m) => m ()
head = el "title" $ text "Reflex Chart Examples"

body :: (DomBuilder t m, PostBuild t m) => m ()
body = el "p" $ text "Written in Reflex"
        -- barWidthChart . join =<< prerender (pure (pure mempty)) fetchCsv
