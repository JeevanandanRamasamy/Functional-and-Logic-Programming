module RegexP where
import Control.Applicative
import Parsing
import Regex
parseRE :: String -> Maybe (RE Char)
parseRE = parse pRE . tokens
getRE :: String -> RE Char
getRE = maybe RNone id . parseRE
data Token symbol
    = TSymbol symbol
    | TE
    | TN
    | TB
    | TP
    | TS
    | TLP
    | TRP
    | TErr
    deriving (Show, Eq)
-- Our parser works on a stream of tokens rather than characters directly.
-- This allows us to defer handling of whitespace and multicharacter symbols
-- to a tokenizer.
pRE :: (Eq symbol) => Parser (Token symbol) (RE symbol)
pRE = assocr (RChoice <$ pSym TB)
    $ assocr (pure RCompos)
    $ postfix pStar
    $ REmpty <$ pSym TE
  <|> RNone <$ pSym TN
  <|> pWhereMap getSymbol
  <|> pSym TLP *> pRE <* pSym TRP
    where
    pStar = RMany <$ pSym TS <|> RSome <$ pSym TP
    getSymbol (TSymbol s) = Just (RSym s)
    getSymbol _ = Nothing
-- Our default tokenizer skips spaces and supports backslash escaping.
-- (Since Haskell also uses backslashes for escaping, we must double them in
-- string and char literals to get actual backslash characters.)
tokens :: String -> [Token Char]
tokens (' ':cs) = tokens cs
tokens ('e':cs) = TE : tokens cs
tokens ('0':cs) = TN : tokens cs
tokens ('(':cs) = TLP : tokens cs
tokens (')':cs) = TRP : tokens cs
tokens ('|':cs) = TB : tokens cs
tokens ('*':cs) = TS : tokens cs
tokens ('+':cs) = TP : tokens cs
tokens ('\\':c:cs) = TSymbol c : tokens cs
tokens ('\\':[]) = [TErr]
tokens (c:cs) = TSymbol c : tokens cs
tokens [] = []