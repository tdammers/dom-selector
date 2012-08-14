{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Text.XML.Selector.TH (jq,queryT) where 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Text.Parsec
import Text.JQuery
import Text.XML.Cursor

import Text.Parsec
import Data.Either.Utils
import Text.XML.Selector.Types
import Text.XML.Selector.Parser

import Data.Maybe
import qualified Data.Map as M

type Parser a = Parsec String () a

-- | Parse jQuery selector string and return a list of 'JQSelector'.
-- parseJQT :: String -> [JQSelector]
-- parseJQT s = either (const []) id (parse parseKey "" (s++" ")) -- (++" ") is super ad hoc!!
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

-- Define QuasiQuoter
jq :: QuasiQuoter
jq = QuasiQuoter jqueryExpr undefined undefined undefined


-- | Get 'Axis' from jQuery selector QQ.
queryT :: [JQSelector] -> Axis
queryT sels = searchTree sels



