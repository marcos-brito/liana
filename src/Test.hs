{-# LANGUAGE OverloadedStrings #-}

module Test where
import qualified Data.Text as T 

testComms :: T.Text -> T.Text
testComms =
  T.unlines
    . map (T.drop 2 . T.strip)
    . filter (T.isPrefixOf "//")
    . T.lines
