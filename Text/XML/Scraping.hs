{-# LANGUAGE OverloadedStrings #-}

-- |Scraping (innerHTML/innerText) and modification (node removal) functions.
module Text.XML.Scraping where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML as X
import Text.XML.Cursor
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
import Text.XML.Selector.Types

-- * InnerHTML / InnerText

-- |Get ''innerHTML'' from a list of cursors.
innerHtml :: [Cursor] -> TL.Text
innerHtml cs = renderNodes $ map node $ concatMap child cs

-- |Get ''innerText'' from a list of cursors.
innerText :: [Cursor] -> T.Text
innerText cs = T.concat $ map (innerTextN . node) cs
-- |''toHTML'' of a list of nodes.
renderNodes :: [Node] -> TL.Text
renderNodes ns = TL.concat $ map (renderHtml . Bl.toHtml) ns

-- |''toHTML'' of a list of cursors.
toHtml :: [Cursor] -> TL.Text
toHtml cs = renderNodes $ map node cs

-- |''innerText'' of a single node.
innerTextN :: Node -> T.Text
innerTextN (NodeElement (Element _ _ cs)) = T.concat $ map innerTextN cs
innerTextN (NodeContent txt) = txt
innerTextN _ = ""

-- * Attirbutes
-- |Tag name of element node. Return Nothing if the node is not an element.
ename :: Node -> Maybe T.Text
ename (NodeElement (Element n _ _)) = Just $ nameLocalName n
ename _ = Nothing

-- |Return an element id. If node is not an element or does not have an id, return Nothing.
eid :: Node -> Maybe T.Text
eid (NodeElement (Element _ as _)) = M.lookup "id" as
eid _ = Nothing

-- |Return element classes. If node is not an element or does not have a class, return an empty list.
eclass :: Node -> [T.Text]
eclass (NodeElement (Element _ as _)) = maybe [] T.words $ M.lookup "class" as
eclass _ = []

-- | Search a meta with a specified name under a cursor, and get a ''content'' field. 
getMeta :: T.Text -> Cursor -> [T.Text] 
getMeta n cursor = concat $ cursor $// element "meta" &| attributeIs "name" n &.// attribute "content"


-- * Removing Nodes
-- |Remove descendant nodes that satisfie predicate (''Destructive'').
remove :: (Node->Bool)->Node->Node
remove f (NodeElement (Element a b cs)) = NodeElement (Element a b (map (remove f) (filter (not . f) cs)))
remove _ n = n

-- |Similar to 'remove', but with a depth.
removeDepth :: (Node->Bool)->Int->Node->Node
removeDepth _ (-1) n = n
removeDepth f d (NodeElement (Element a b cs)) = NodeElement (Element a b (map (removeDepth f (d-1)) (filter (not . f) cs)))
removeDepth _ _ n = n

-- |Remove elements with specified tags.
removeTags :: [T.Text] -> [Node] -> [Node]
removeTags ts ns = map (remove (\n -> ename n `elem` map Just ts)) ns

-- |Remove descendant nodes that match a query string.
removeQuery :: String -> [Node] -> [Node]
removeQuery q ns = map (remove (queryMatchNode q)) ns

-- |Remove descendant nodes that match any of query strings.
removeQueries :: [String] -> [Node] -> [Node]
removeQueries qs ns = map (remove f) ns
  where
    f :: Node -> Bool
    f n = any (flip queryMatchNode n) qs

-- |See if the node contains any descendant (and self) node that satisfies predicate.
-- To return false, this function needs to traverse all descendant elements....
nodeHaving :: (Node->Bool)->Node->Bool
nodeHaving f n@(NodeElement (Element _ _ cs)) = f n || any (nodeHaving f) cs
nodeHaving _ _ = False


-- |Remove descendant nodes that match the condition (similar to 'remove')
rmElem :: String -> String -> [String] -> [Node] -> [Node]
rmElem tag id kl ns = map (remove f) ns
  where
    f :: Node -> Bool
    f (NodeElement e) = selectorMatch (JQSelector Descendant (g tag) (g id) kl []) e
    f _ = False
    g :: String -> Maybe String
    g "" = Nothing
    g s = Just s


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


