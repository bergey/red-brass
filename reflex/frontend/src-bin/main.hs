{-# LANGUAGE OverloadedStrings #-}
import SchoolDistricts (barWidthChart, fetchCsv)

-- import Frontend
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
    el "p" $ text "Written in Reflex"
    barWidthChart =<< fetchCsv
