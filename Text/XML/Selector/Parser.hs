--
-- Parsers for CSS query strings
--
{-# LANGUAGE DoAndIfThenElse #-}

module Text.XML.Selector.Parser (parseJQ) where
import Text.Parsec
-- import Data.Either.Utils
import Text.XML.Selector.Types

import Data.Maybe
import qualified Data.Map as M

type Parser a = Parsec String () a

-- | Parse a jQuery selector string and return a list of 'JQSelector'.
parseJQ :: String -> [JQSelector]
parseJQ s = either (const []) id (parse parseKey "" (s++" ")) -- (++" ") is super ad hoc!!

-- test_parseQuery s = forceEither $ parse parseKey "" (s++" ") 

data JQSelectorToken = JQSelectorToken {
	rel :: RelPrev,
	tagNameIdClassAttr :: [NameIdClassAttr]
} deriving (Eq,Show)

transformSelector :: JQSelectorToken -> JQSelector
transformSelector (JQSelectorToken rel name) = JQSelector rel (t1 t) (t2 t) (t3 t) (t4 t)
  where
    t = f name (Nothing,Nothing,[],[])
    f [] r = r
    f ((TagName s):xs) r = f xs (Just s,t2 r,t3 r,t4 r)
    f ((Id s):xs) r = f xs (t1 r,Just s,t3 r,t4 r)
    f ((Class s):xs) r = f xs (t1 r,t2 r,s:t3 r,t4 r)
    f ((Attr k op v):xs) r = f xs (t1 r,t2 r,t3 r,(g k op v):(t4 r))
    t1 (a,_,_,_) = a
    t2 (_,a,_,_) = a
    t3 (_,_,a,_) = a
    t4 (_,_,_,a) = a
    g k op v = TagAttr k v (fromMaybe Exists (op >>= (flip M.lookup attrOpList)))
    
attrOpList :: M.Map String AttrRel
attrOpList = M.fromList [("=",Equal),("|=",Contains),("!=",NotEqual),("^=",Begin),("$=",End),("*=",ContainsWord)]

parseKey :: Parser [JQSelector]
parseKey = many1 selector

selector :: Parser JQSelector
selector = do
  skipMany myspaces
  sep <- optionMaybe (choice (map char ">+~"))
  let t = case sep of
            Just '>' -> Child
            Just '+' -> Next
            Just '~' -> Sibling
            Nothing -> Descendant
            _ -> error "Incorrect option."
  skipMany myspaces
  tok <- many1 $ choice [try selId, try selClass, try selTag, try selAttr]
  skipMany myspaces
  return $ transformSelector (JQSelectorToken t tok)

data NameIdClassAttr = TagName String | Id String | Class String | Attr String (Maybe String) (Maybe String) deriving (Eq,Show,Ord)

selTag :: Parser NameIdClassAttr
selTag = do
	s <- many1 cssChar
	return $ TagName s

selId :: Parser NameIdClassAttr
selId = do
  char '#'
  s <- many1 cssChar
  return $ Id s

selClass :: Parser NameIdClassAttr
selClass = do
	char '.'
	s <- many1 cssChar
	return $ Class s

selAttr :: Parser NameIdClassAttr
selAttr = do
  char '['
  k <- many1 cssChar
  op <- optionMaybe attrOp
  q <- optionMaybe (oneOf "\"'")
  v <- optionMaybe $ many1 (noneOf (maybe "]" (:[]) q))
  if isJust q then do
    char (fromJust q)
  else do
    return ' '
  char ']'
  return (Attr k op v)

-- stopDelim = (lookAhead (choice (map char ".#>+~ \t")))

--
-- Tokens
--
-- myspaces :: Stream s m Char => ParsecT s u m Char
myspaces = choice (map char " \t\r\n")

-- attrOp :: Stream s m Char => ParsecT s u m String
attrOp = choice $ map string ["=","|=","!=","*=","$=","^="]

-- cssChar :: Stream s m Char => ParsecT s u m Char
cssChar = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"





