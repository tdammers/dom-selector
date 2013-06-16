{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Text.XML.Selector.TH (jq,queryT) where 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Text.Parsec
import Text.XML.Selector
import Text.XML.Cursor

import Text.Parsec
import Text.XML.Selector.Types
import Text.XML.Selector.Parser

import Data.Maybe
import qualified Data.Map as M

type Parser a = Parsec String () a

$(deriveLift ''JQSelector)
$(deriveLift ''TagAttr)
$(deriveLift ''RelPrev)
$(deriveLift ''AttrRel)

-- instance Lift JQSelector where
--  lift (JQSelector a b c d e) = [| JQSelector a b c d e |]

jqueryExpr :: String -> Q Exp
jqueryExpr str = do
  case parseJQ str of
                 [] -> error "query: Invalid selector"
                 sels -> [| sels |]

-- | QuasiQuoter for CSS selector
jq :: QuasiQuoter
jq = QuasiQuoter jqueryExpr undefined undefined undefined


-- |Get 'Axis' from jQuery selector QQ.
--
-- >html = innerHtml $ cursor $| queryT [jq| ul.foo > li#bar |]
queryT :: [JQSelector] -> Axis
queryT sels = searchTree sels


