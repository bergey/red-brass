{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend where

import BarChart (barChart)
import Scatter (scatterPlot)
import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
    { _frontend_head = el "title" $ text "Reflex Chart Examples"
    , _frontend_body = do
        -- text "Welcome to Obelisk!"
        scatterPlot
        barChart
  }
