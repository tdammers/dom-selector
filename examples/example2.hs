-- Minimum parsing and extracting DOM.

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Text.XML.Cursor
import qualified Text.HTML.DOM as H (readFile)
import qualified Data.Text.Lazy.IO as TI (putStrLn,writeFile)
import Text.XML.Scraping
import Text.XML.Selector.TH
import Control.Monad(forM_)

main :: IO ()
main = do
  c <- fmap fromDocument $ H.readFile "input2.html"
  forM_ [0..(length axes)-1] (test c)

test :: Cursor -> Int -> IO ()
test c n = do
  let cs = (axes !! n) c
  (putStrLn . (++" elements") . show . length) cs
  TI.writeFile ("output2."++(show n)++".html") (toHtml cs)


axes :: [Axis]
axes = map queryT [
  [jq| ul > li|]
  , [jq| div div li|]
  , [jq| ul|]
  , [jq| ul + ul|]
  , [jq| li.Japan ~ li|]
  , [jq| [marked] |]
  , [jq| li[favorite='1'] |]
  ]

