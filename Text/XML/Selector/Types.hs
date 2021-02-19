module Text.XML.Selector.Types where 

import Data.List (sort)
import qualified Data.Map as M (lookup,fromList)
import Data.Maybe (fromMaybe)

-- |JQSelector represents one token of jquery selector. One JQSelector corresponds to \"div#content\", \"a[href='/index.html']\", etc. You can get a list of JQSelector by 'parseJQ', and show them by 'showJQ'
-- As long as you use 'query', you don't need to handle this type directly.
data JQSelector = JQSelector {
  relPrev :: RelPrev,
  jqTagName :: Maybe String,
  jqTagId :: Maybe String,
  jqTagClass :: [String],
  jqTagAttr :: [TagAttr],
  jqSelfFilters :: [SelfFilter]
} deriving (Show,Read,Ord)

instance Eq JQSelector where
  (JQSelector r1 n1 i1 c1 a1 f1) == (JQSelector r2 n2 i2 c2 a2 f2) =
    r1 == r2 &&
    n1 == n2 &&
    i1 == i2 &&
    sort c1 == sort c2 &&
    sort a1 == sort a2 &&
    sort f1 == sort f2

data TagAttr = TagAttr {
  attrName :: String,
  attrVal :: Maybe String,
  attrRel :: AttrRel
} deriving (Eq,Show,Read,Ord)

data AttrRel = Equal | Begin | End | Contains | NotEqual | ContainsWord | Exists deriving (Show,Eq,Ord,Enum,Read)
relToStr :: AttrRel -> String
relToStr r = fromMaybe "N/A" $ M.lookup r (M.fromList [(Equal,"="),(Begin,"^="),(End,"$="),(Contains,"|="),(NotEqual,"!="),(ContainsWord,"*=")])

-- |Relationship to the preceding selector.
data RelPrev = Descendant | Child | Next | Sibling deriving (Eq,Show,Enum,Read,Ord)

-- |Various filters
data SelfFilter = FirstChild | LastChild deriving (Eq,Show,Enum,Read,Ord)
