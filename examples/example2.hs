-- Minimum parsing and extracting DOM.

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Text.XML.Cursor
import qualified Text.HTML.DOM as H (readFile)
import qualified Data.Text.Lazy.IO as TI (putStrLn,writeFile)
import Text.XML.Scraping
import Text.XML.Selector
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


-- Axis is a Cursor -> [Cursor] type. Cursor represents one position in a DOM tree.
-- Axis takes a cursor and returns a traversing result as a list of cursors.
-- For details, see Text.XML.Cursor in xml-conduit package.
-- jq quasiquote parses a CSS selector at compile time and queryT [jq| ... |] generates an Axis.
-- query "..." is equivalent of queryT but takes a string as a parameter. 

-- axes is a list of Axis.
axes :: [Axis]
axes = map queryT [
  [jq| ul > li|]
  , [jq| div div li|]
  , [jq| ul|]
  , [jq| ul + ul|]
  , [jq| li.Japan ~ li|]
  , [jq| [marked] |]
  , [jq| li[favorite='1'] |]
  ] ++
  map query ["ul > li", "div div li"]

