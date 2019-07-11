{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
    { _frontend_head = el "title" $ text "Obelisk Minimal Example"
    , _frontend_body = do
        text "Welcome to Obelisk!"
        elAttr "svg"
          ( "height" =: "500" <> "width" =: "500" <> "style" =: "border: 1px solid cyan; display: block" ) $ do
            elAttr "line" ( "x1" =: "49" <> "y1" =: "50" <> "x2" =: "150" <> "y2" =: "50" <> "stroke" =: "black" ) blank
  }
