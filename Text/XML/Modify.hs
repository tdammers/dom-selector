{-# LANGUAGE OverloadedStrings #-}


module Text.XML.Modify where

-- import Import
import qualified Data.Text as T
import Text.XML as X
import Text.XML.Cursor
import qualified Text.XML.Cursor.Generic as CG
-- import Data.Either.Utils
import Data.String
import qualified Text.HTML.DOM as H
import qualified Data.Map as M
import Data.List
import Data.Maybe
import System.Environment (getArgs)
import Text.Blaze.Html as Bl
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy  (fromStrict,toStrict, unpack)

import Text.XML.Selector

ename :: Node -> Maybe T.Text
ename (NodeElement (Element n _ _)) = Just $ nameLocalName n
ename _ = Nothing

-- Remove descendant nodes that satisfie predicate ("Destructive")
remove :: (Node->Bool)->Node->Node
remove f (NodeElement (Element a b cs)) = NodeElement (Element a b (map (remove f) (filter (not . f) cs)))
remove _ n = n

removeDepth :: (Node->Bool)->Int->Node->Node
removeDepth _ (-1) n = n
removeDepth f d (NodeElement (Element a b cs)) = NodeElement (Element a b (map (removeDepth f (d-1)) (filter (not . f) cs)))
removeDepth _ _ n = n

removeTags :: [T.Text] -> [Node] -> [Node]
removeTags ts ns = map (remove (\n -> ename n `elem` map Just ts)) ns

-- Remove descendant nodes that match a query string.
removeQuery :: String -> [Node] -> [Node]
removeQuery q ns = map (remove (queryMatchNode q)) ns

-- Remove descendant nodes that match any of query strings.
removeQueries :: [String] -> [Node] -> [Node]
removeQueries qs ns = map (remove f) ns
  where
    f :: Node -> Bool
    f n = any (flip queryMatchNode n) qs

-- See if the node contains any descendant (and self) node that satisfies predicate.
-- To return false, this function needs to traverse all descendant elements....
nodeHaving :: (Node->Bool)->Node->Bool
nodeHaving f n@(NodeElement (Element _ _ cs)) = f n || any (nodeHaving f) cs
nodeHaving _ _ = False


renderNodes :: [Node] -> T.Text
renderNodes ns = T.concat $ map (toStrict . renderHtml . Bl.toHtml) ns

toHtml :: [Cursor] -> T.Text
toHtml cs = renderNodes $ map node cs

innerHtml :: [Cursor] -> T.Text
innerHtml cs = renderNodes $ map node $ concatMap child cs

innerText :: [Cursor] -> T.Text
innerText cs = T.concat $ map (innerTextN . node) cs

nodeId :: Node -> Maybe T.Text
nodeId (NodeElement (Element _ as _)) = M.lookup "id" as
nodeId _ = Nothing

innerTextN :: Node -> T.Text
innerTextN (NodeElement (Element _ _ cs)) = T.concat $ map innerTextN cs
innerTextN (NodeContent txt) = txt
innerTextN _ = ""

getMeta :: T.Text -> Cursor -> [T.Text] 
getMeta n cursor = concat $ cursor $// element "meta" &| attributeIs "name" n &.// attribute "content"



--
-- Conversion to trees
--
{-
toTree :: Node -> Tr.Tree String
toTree (TextNode t) = Tr.Node ("\""++T.unpack t++"\"") []
toTree (Comment t) = Tr.Node ("<-- "++T.unpack t++" -->") []
toTree (Element t as c) = Tr.Node str (map toTree c)
	where
		str = T.unpack t ++ g as
		g as | null as = ""
					| otherwise = ": [" ++ intercalate "," (map f as) ++ "]"
		f (k,v) = T.unpack k ++ "=\"" ++ T.unpack v ++ "\""


showTree :: [Node] -> IO ()
showTree ns = do
	mapM_ (putStr . Tr.drawTree . toTree) ns
-}
--
-- Helper functions for XmlHtml data
--
{-
(!!!) :: Node -> Int -> Node
(Element _ _ c) !!! i = c !! i
_ !!! _ = error "No child elements"
-}

-- Following is jQuery module dependent
--

rmElem :: String -> String -> [String] -> [Node] -> [Node]
rmElem tag id kl ns = map (remove f) ns
  where
    f :: Node -> Bool
    f (NodeElement e) = selectorMatch (JQSelector Descendant (g tag) (g id) kl []) e
    f _ = False
    g :: String -> Maybe String
    g "" = Nothing
    g s = Just s



