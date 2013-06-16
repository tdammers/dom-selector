-- Minimum parsing and extracting DOM.

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Text.XML.Cursor
import Text.HTML.DOM as H
import qualified Data.Text.Lazy.IO as TLI (putStrLn)
import qualified Data.Text.IO as TI (putStrLn)
import Text.XML.Scraping
import Text.XML.Selector.TH
import Data.Conduit as C

import Network.HTTP.Conduit
import Data.Conduit.Binary

import Control.Monad (forM_)

main = do
  main1
  main2
  main3

-- readFile function from html-conduit
main1 :: IO ()
main1 = do
  c <- fmap fromDocument $ H.readFile "input.html"
  let cs = queryT [jq| ul#foo > li.bar |] c
  TLI.putStrLn $ innerHtml cs

-- Explicit use of conduit
main2 = do
  doc <- C.runResourceT $ sourceFile "input.html" $$ sinkDoc
  let cs = queryT [jq| ul#foo > li.bar |] (fromDocument doc)
  TLI.putStrLn $ innerHtml cs

-- simpleHttp from http-conduit
main3 = do
  content <- simpleHttp "http://ja.wikipedia.org/wiki/Haskell"
  let doc = parseLBS content
  let cs = queryT [jq| div#p-lang li |] (fromDocument doc)
  forM_ cs $ \c -> do
    TLI.putStrLn $ innerText c
  putStrLn $ (show $ length cs) ++ " languages."

