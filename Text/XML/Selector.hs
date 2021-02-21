{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, TemplateHaskell, QuasiQuotes #-}

--  CSS selector for Text.XML (xml-conduit) library.
--
--
-- Examples
-- doc <- H.readFile "input.html"
-- let c = fromDocument c
-- c $| query
-- c $| query "meta[name='dc.Creator']" >=> attribute "content"

-- |This module has query functions for traversing DOM. 'queryT', a quasiquote version, is also available in "Text.XML.Selector.TH" module.
module Text.XML.Selector (
  query,
  query1,
  searchTree,
  showJQ,
  byId,
  byClass,
  selectorMatch,
  next,
  maybeText,
  headm,
  queryMatchNode
  )
where

-- import Import
-- import qualified Data.Tree as Tr
import Data.List
import qualified Data.Text as T
import Text.XML.Cursor
import Text.XML as X -- hiding (Name)
-- import Data.Either.Utils
import qualified Data.Map as M
import Data.Maybe
-- import Language.Haskell.TH (runQ)
import Text.XML.Selector.Parser
import Text.XML.Selector.Types
import Debug.Trace


-- | Show a parsed selector.
-- (parseJQ . showJQ) == id
showJQ :: [JQSelector] -> String
showJQ ss = foldl f "" ss
  where
    f str sel = str ++ g (relPrev sel) ++ 
      maybe "" id (jqTagName sel) ++
      maybe "" (("#"++)) (jqTagId sel) ++ 
      concat (map (("."++)) (jqTagClass sel)) ++
      if null (jqTagAttr sel) then "" else (concatMap h (jqTagAttr sel)) ++
      concatMap p (jqSelfFilters sel)
    g Descendant = " "
    g Child = " > "
    g Next = " + "
    g Sibling = " ~ "
    h (TagAttr k Nothing _) = "["++k++"]"
    h (TagAttr k (Just v) r) = "["++k ++ relToStr r ++ "\"" ++ v ++ "\"]"
--    h _ = error "Invalid TagAttr"
    p FirstChild = ":first-child"
    p LastChild = ":last-child"


-- Some elementary search

-- | Axis for choosing elements by an id
byId :: String -> Axis
byId s = checkElement (elemHasId (Just s))

-- byIdT :: String -> Axis
byIdT s = [e| checkElement (elemHasId (Just s)) |]


-- | Axis for choosing elements by a class
byClass :: String -> Axis
byClass s = checkElement (elemHasClass [s])

-- Some traversing

-- |Gets the next sibling. Note that this is not a Axis.
next :: Cursor -> Maybe Cursor
next c = headm (c $| followingSibling)


-- Auxiliary functions
headm :: [a] -> Maybe a
headm [] = Nothing
headm (x:_) = Just x

take1' :: [a] -> [a]
take1' xs = if null xs then [] else take 1 xs

maybeText :: T.Text -> Maybe T.Text
maybeText "" = Nothing
maybeText t = Just t

--
-- Traversing by jQuery string
--


-- | Get 'Axis' from jQuery selector string.
query :: String -> Axis
query keystr = case parseJQ keystr of
                 [] -> error "query: Invalid selector"
                 sels -> searchTree sels


-- | Return Just (the first element of query results). If no element matches, it returns Nothing.
query1 :: String -> Cursor -> Maybe Cursor
query1 s n | null res = Nothing
	| otherwise = Just (head res)
	where res = query s n

{-
-- | Old version: search direction should be child -> parent to avoid duplicates for nested elements with same tag.
searchTreeOld :: [JQSelector] -> Axis
searchTreeOld [] c = [c]
searchTreeOld (x@(JQSelector Descendant _ _ _ _):xs) c = c $// checkElement (selectorMatch x)  >=> searchTree xs
searchTreeOld (x@(JQSelector Child _ _ _ _):xs) c = c $/ checkElement (selectorMatch x) >=> searchTree xs
searchTreeOld (x@(JQSelector Next _ _ _ _):xs) c =
  case c $| followingSibling >=> anyElement of
    [] -> []
    cs -> (head cs) $| checkElement (selectorMatch x) >=> searchTree xs
searchTreeOld ((JQSelector Sibling _ _ _ _):xs) c =  c $| followingSibling >=> searchTree xs
-}

searchTree :: [JQSelector] -> Axis
searchTree xs = search (reverse xs)
  where
    search [] c = [c]
    search (x@(JQSelector rel _ _ _ _ fil):xs) c =
      c $// checkElement (selectorMatch x)
        >=> check (traceAncestors rel xs)
        >=> traceSelfFilters fil

traceSelfFilters :: [SelfFilter] -> Axis
traceSelfFilters [] = (:[])
traceSelfFilters (f:fs) = traceSelfFilter f >=> traceSelfFilters fs

traceSelfFilter :: SelfFilter -> Axis
traceSelfFilter FirstChild c =
  if null (precedingSibling c) then [c] else []
traceSelfFilter LastChild c =
  if null (followingSibling c) then [c] else []

traceAncestors :: RelPrev -> [JQSelector] -> Axis
traceAncestors Child (x:xs) c
  | isJust p = if matchCursor x (fromJust p) then traceAncestors (relPrev x) xs (fromJust p) else []
  | otherwise = []
    where
      p :: Maybe Cursor
      p = headm (parent c)
traceAncestors Descendant (x:xs) c
  = case filter (matchCursor x) (ancestor c) of
      [] -> []
      as -> concatMap (traceAncestors (relPrev x) xs) as
traceAncestors Next (x:xs) c
  | isJust p = if matchCursor x (fromJust p) then traceAncestors (relPrev x) xs (fromJust p) else []
  | otherwise = []
    where
      p :: Maybe Cursor
      p = headm (precedingSibling c)
traceAncestors Sibling (x:xs) c
  = case filter (matchCursor x) (precedingSibling c) of
      [] -> []
      as -> concatMap (traceAncestors (relPrev x) xs) as
traceAncestors _ [] c = [c]


-- |Return if an element matches a selector
selectorMatch :: JQSelector -> Element -> Bool
selectorMatch (JQSelector _ name id klass attr _) e
  = elemIsTag name e && elemHasId id e && elemHasClass klass e && all (flip elemHasAttr e) attr

matchNode :: JQSelector -> Node -> Bool
matchNode sel (NodeElement elem) = selectorMatch sel elem
matchNode _ _ = False

matchCursor :: JQSelector -> Cursor -> Bool
matchCursor sel cursor = matchNode sel (node cursor)

-- |Return if a node matches a selector given by string
-- |Only first token is used (i.e. no hierarchy is enabled.)
queryMatchNode :: String -> Node -> Bool
queryMatchNode s (NodeElement e)
    = case qq of
        Just q -> selectorMatch q e
        Nothing -> False
  where qq = headm $ parseJQ s
queryMatchNode _ _ = False


-- return False when tag is specified in selector (1st arg) but not the one in the element (2nd arg)
elemIsTag :: Maybe String -> Element -> Bool
elemIsTag Nothing _ = True
elemIsTag (Just tag) (Element n _ _) = nameLocalName n == T.pack tag

elemHasId :: Maybe String -> Element -> Bool
elemHasId Nothing _ = True
elemHasId (Just id) (Element _ as _) = M.lookup "id" as == Just (T.pack id)

elemHasClass :: [String] -> Element -> Bool
elemHasClass [] _ = True
elemHasClass ks (Element _ as _) = all (`elem` kl) $ map T.pack ks
  where
      kl = maybe [] T.words (M.lookup "class" as)


-- This is different from the above three funcs: by default return False
elemHasAttr :: TagAttr -> Element -> Bool
elemHasAttr attr (Element _ as _) = relFunc (attrRel attr) (g as) (Just (fromMaybe "" (attrVal attr)))
  where
    relFunc :: AttrRel -> Maybe String -> Maybe String -> Bool
    relFunc Equal (Just a) (Just b) = a == b
    relFunc Exists a _ = isJust a   -- '[checked]'
    relFunc Contains (Just a) (Just b) = b `isInfixOf` a
    relFunc Begin (Just a) (Just b) = b `isPrefixOf` a
    relFunc End (Just a) (Just b) = b `isSuffixOf` a
    relFunc NotEqual (Just a) (Just b) = a /= b
    relFunc ContainsWord (Just a) (Just b) = all (`isInfixOf` a) (words b)
    relFunc _ _ _ = False
--    relFunc r a b = error ("Attribute query invalid pattern: " ++ show attr ++ " : " ++ show r ++ show a ++ show b)
--    nameToStr :: M.Map Name T.Text -> M.Map String T.Text
--    nameToStr m = trace (show m) m
    toName s = Name (T.pack s) Nothing Nothing
    g :: M.Map Name T.Text -> Maybe String
    g as = fmap T.unpack (M.lookup (toName $ attrName attr) as)
    

-- elemClass :: Element -> [T.Text]
-- elemClass (Element _ a _) = concat $ maybeToList $ fmap T.words $ (M.lookup "class" a)


