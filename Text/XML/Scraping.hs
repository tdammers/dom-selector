{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

-- |Scraping (innerHTML/innerText) and modification (node removal) functions.
module Text.XML.Scraping (
  -- * InnerHTML / InnerText
  GetInner (..)
  -- * Attirbutes
  , GetAttribute (..)
  -- * Removing descendant nodes
  -- |These functions work on 'Node' or [Node]
  , remove
  , removeDepth
  , removeTags
  , removeQueries
  , rmElem
  -- * Other
  , nodeHaving
  -- * Deprecated
  , removeQuery
) where

import Data.Text (Text)
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
import qualified Text.Blaze.Html as Bl
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy  (fromStrict,toStrict, unpack)

import Text.XML.Selector
import Text.XML.Selector.Types


-- |Type class for getting lazy text representation of HTML element(s). This can be used for 'Node', 'Cursor', [Node], and [Cursor].
class GetInner elem where
  -- | ''innerHtml'' of the element(s).
  innerHtml :: elem -> TL.Text
  -- | ''innerText'' of the element(s).
  innerText :: elem -> TL.Text
  -- | ''toHtml'' of the element(s).
  toHtml :: elem -> TL.Text

instance GetInner Node where
  innerHtml = TL.concat . map toHtml . child . fromNode
  
  innerText (NodeElement (Element _ _ cs)) = (TL.concat . map innerText) cs
  innerText (NodeContent txt) = fromStrict txt
  innerText _ = ""

  toHtml = renderHtml . Bl.toHtml

instance GetInner Cursor where
  innerHtml = TL.concat . map (toHtml . node) . child
  innerText = innerText . node
  toHtml = toHtml . node

instance (GetInner a) => GetInner [a] where
  innerHtml = TL.concat . map innerHtml
  innerText = TL.concat . map innerText
  toHtml = TL.concat . map toHtml

-- * Attirbutes

class GetAttribute elem where
  -- |Tag name of element node. Returns Nothing if the node is not an element.
  ename :: elem -> Maybe Text
  -- |Returns an element id. If node is not an element or does not have an id, returns Nothing.
  eid :: elem -> Maybe Text
  -- |Returns element classes. If node is not an element or does not have a class, returns an empty list.
  eclass :: elem -> [Text]
  -- | Searches a meta with a specified name under a cursor, and gets a ''content'' field. 
  getMeta :: Text -> elem -> [Text] 

instance GetAttribute Node where
  ename (NodeElement (Element n _ _)) = Just $ nameLocalName n
  ename _ = Nothing

  eid (NodeElement (Element _ as _)) = M.lookup "id" as
  eid _ = Nothing

  eclass (NodeElement (Element _ as _)) = maybe [] T.words $ M.lookup "class" as
  eclass _ = []

  getMeta n = getMeta n . fromNode

instance GetAttribute Cursor where
  ename = ename . node
  eid = eid . node
  eclass = eclass . node
  getMeta n cursor = concat $ cursor $// element "meta" &| attributeIs "name" n &.// attribute "content"


-- * Removing Nodes

{-
data ScrapingOp = RemoveOp String
data Scraping a = Scraping {
  ops :: [ScrapingOp],
  runScraping :: a -> a
}

instance Monad Scraping where
  a >>= f = 
-}
-- 
--
--

-- |Removes descendant nodes that satisfy predicate, and returns a new updated 'Node'.
-- This is a general function, and internally used for other remove* functions in this module.
remove :: (Node->Bool)->Node->Node
remove f (NodeElement (Element a b cs)) = NodeElement (Element a b (map (remove f) (filter (not . f) cs)))
remove _ n = n

-- |Similar to 'remove', but with a limit of depth.
removeDepth :: (Node->Bool)->Int->Node->Node
removeDepth _ (-1) n = n
removeDepth f d (NodeElement (Element a b cs)) = NodeElement (Element a b (map (removeDepth f (d-1)) (filter (not . f) cs)))
removeDepth _ _ n = n

-- |Remove all descendant nodes with specified tag names.
removeTags :: [String] -> [Node] -> [Node]
removeTags ts ns = map (remove (\n -> ename n `elem` map (Just . T.pack) ts)) ns

-- | Remove all descendant nodes that match any of query strings.
-- ''removeQuery'' in ver 0.1 was merged into this.
removeQueries :: [String] -> [Node] -> [Node]
removeQueries [q] ns = map (remove (queryMatchNode q)) ns
removeQueries qs ns = map (remove f) ns
  where
    f :: Node -> Bool
    f n = any (flip queryMatchNode n) qs

{-# DEPRECATED removeQuery "Use removeQueries instead." #-}
-- | Remove all descendant nodes that match a query string.
removeQuery :: String -> [Node] -> [Node]
removeQuery q ns = removeQueries [q] ns

-- |Checks whether the node contains any descendant (and self) node that satisfies predicate.
-- To return false, this function needs to traverse all descendant elements, so this is not efficient.
nodeHaving :: (Node->Bool)->Node->Bool
nodeHaving f n@(NodeElement (Element _ _ cs)) = f n || any (nodeHaving f) cs
nodeHaving _ _ = False

-- |Remove descendant nodes that match specified tag, id, and class (similar to 'remove', but more specific.)
--  If you pass an empty string to tag or id, that does not filter tag or id (Read the source code for details).
--
-- @
-- rmElem ''div'' ''div-id'' [''div-class'', ''div-class2''] nodes = newnodes
-- @
rmElem :: String -> String -> [String] -> [Node] -> [Node]
rmElem tag id kl ns = map (remove f) ns
  where
    f :: Node -> Bool
    f (NodeElement e) = selectorMatch (JQSelector Descendant (g tag) (g id) kl [] []) e
    f _ = False
    g :: String -> Maybe String
    g "" = Nothing
    g s = Just s

{-
-- Not yet finished. ToDo: Look at State monad. This should be similar.
type Query = String
data ScrapingOp = Remove Query

data Scraping = Scraping [ScrapingOp]

removeBy :: String -> Scraping

instance Functor Scraping where
  fmap f (Scraping ops) = Scraping (map f ops)

instance Applicative Scraping where
  pure ops = Scraping ops
  Scraping f <*> Scraping a = Scraping (f a)

instance Monad Scraping where
  return = pure
  Scraping a >>= f = Scraping (


-}
