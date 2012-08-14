-- Minimum parsing and extracting DOM.

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Text.XML.Cursor
import qualified Text.HTML.DOM as H (readFile)
import qualified Data.Text.Lazy.IO as TI (putStrLn)
import Text.XML.Scraping
import Text.XML.Selector.TH

main :: IO ()
main = do
  c <- fmap fromDocument $ H.readFile "input.html"
  let cs = queryT [jq| ul#foo > li.bar |] c
  TI.putStrLn $ innerHtml cs

