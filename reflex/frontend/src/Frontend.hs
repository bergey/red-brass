{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend where

import BarChart (barChart)
import ChartUtil
import Common.Api
import Common.Route
import Obelisk.Generated.Static
import Obelisk.Route.Frontend (RoutedT)
import Scatter (scatterPlot)
import SchoolDistricts (barWidthChart, fetchCsv)

import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Control.Monad
import Data.Maybe (fromMaybe)
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List (elemIndex, sortOn)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
    { _frontend_head = el "title" $ text "Reflex Chart Examples"
    , _frontend_body = do
        barWidthChart . join =<< prerender (pure (pure mempty)) fetchCsv
  }
