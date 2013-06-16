-- test-scraping.hs
{-# LANGUAGE QuasiQuotes,OverloadedStrings,DoAndIfThenElse #-}

module Main where

import System.Exit (exitFailure)

import Text.XML.Cursor
import Text.HTML.DOM as H
import Text.XML.Scraping
import Text.XML.Selector.TH
import Data.List
import Control.Monad
import qualified Data.Text.Lazy.IO as TLIO

main = do
  root <- fmap fromDocument $ H.readFile "examples/input2.html"
  mapM_ testOne (props root)
  props2 root
  return ()

testOne :: (String,Bool) -> IO ()
testOne (name,pred) = do
  if pred then do
    putStrLn $ "OK: " ++ name
    return ()
  else
  	exitFailure

props c = [
	("div li", length (queryT [jq| li |] c) == length (queryT [jq| div li |] c))
	,("ul li",length (queryT [jq| ul li |] c) == length (queryT [jq| ul > li |] c))
	,("inner",innerText (queryT [jq| #evening |] c) == (innerText . map node) (queryT [jq| #evening |] c))
	,("inner2",(innerText . head) (queryT [jq| #evening |] c) ==
		(innerText . head . map node) (queryT [jq| #evening |] c))
	]

props2 c = do
  TLIO.putStrLn $ (toHtml) (queryT [jq| #evening |] c)
  TLIO.putStrLn $ (toHtml . map node) (queryT [jq| #evening |] c)
  TLIO.putStrLn $ (innerHtml) (queryT [jq| #evening |] c)
  TLIO.putStrLn $ (innerHtml . map node) (queryT [jq| #evening |] c)
  TLIO.putStrLn $ (innerText) (queryT [jq| #evening |] c)
  TLIO.putStrLn $ (innerText . map node) (queryT [jq| #evening |] c)
