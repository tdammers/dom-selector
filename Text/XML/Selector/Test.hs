--
-- Testing
--

module Text.XML.Selector.Test (prop_parseJQ) where

import Text.XML.Selector (showJQ)
import Text.XML.Selector.Types
import Text.XML.Selector.Parser
import Test.QuickCheck
import Control.Monad
import Data.Maybe



-- |QuickCheck for a parser.
prop_parseJQ :: [JQSelector] -> Bool
prop_parseJQ ss = (parseJQ . showJQ) ss == ss


instance Arbitrary JQSelector where
  arbitrary = do
    rel <- arbitrary
    nm <- arbCssId
    a <- choose (0,1)
    let name = [Nothing, Just nm] !! a
    ids <- arbCssId
    c <- choose (0,1)
    let id = [Nothing, Just ids] !! c
    kl <- arbCssId
    b <- choose (0,10)
    klass <- replicateM b arbCssId
    attr <- case (name, id, klass) of
              (Nothing, Nothing, []) -> do
                n <- choose (1,10)
                replicateM n (arbitrary :: Gen TagAttr)
              _ -> arbitrary
    filter <- arbitrary
    return (JQSelector rel name id klass attr filter)

instance Arbitrary TagAttr where
  arbitrary = do
    name <- arbCssId
    rel <- arbitrary
    s <- arbCssId
    let v = Just s
    let val = if rel == Exists then Nothing else v
    return (TagAttr name val rel)

instance Arbitrary RelPrev where
  arbitrary = elements (enumFrom (toEnum 0))

instance Arbitrary AttrRel where
  arbitrary = elements (enumFrom (toEnum 0))

instance Arbitrary SelfFilter where
  arbitrary = elements (enumFrom (toEnum 0))


arbCssId :: Gen String
arbCssId = do
  n <- choose (1,10)
  replicateM n (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")
