-- A framework for defining recursive-descent parsers.
module Parsing where
import Control.Applicative
-- Parser type
-- p :: Parser symbol result
-- p is a parser that reads a string of symbols
-- and produces a result on success.
newtype Parser s a = P ([s] -> [(a, [s])])
unP (P p) = p
-- Using parsers
-- Use the parser to extract a result from a string.
-- Returns Nothing on failure, or if the parse is ambiguous.
parse :: Parser s a -> [s] -> Maybe a
parse p s =
    case parses p s of
        [x] -> Just x
        _ -> Nothing
-- Use the parser to extract all results from a string.
-- Note: we only return results obtained by consuming the entire
-- string.
parses :: Parser s a -> [s] -> [a]
parses (P p) = map fst . filter (null . snd) . p
-- Constructing parsers
-- Read a single symbol
pSym :: (Eq symbol) => symbol -> Parser symbol symbol
pSym s = pWhere (s ==)
-- Read any symbol satisfying the conditional
pWhere :: (symbol -> Bool) -> Parser symbol symbol
pWhere f = P p
    where
    p (s:ss) | f s = [(s,ss)]
    p _ = []
pWhereMap :: (symbol -> Maybe a) -> Parser symbol a
pWhereMap f = P p
    where
    p (s:ss) = case f s of
        Just a -> [(a,ss)]
        Nothing -> []
    p _ = []
instance Functor (Parser s) where
-- Apply a function to the result of another parser
    fmap f (P p) = P (map (\(x,s) -> (f x, s)) . p)
-- implicitly defined
-- f <$> p - reads a string matching p, produces f applied to the value
-- p produced (alias for fmap)
-- x <$ p - reads a string matching p and produces x
instance Applicative (Parser s) where
-- Read no symbols and produce a result immediately
    pure x = P (\s -> [(x,s)])
-- pf <*> px reads a string matching pf followed by
-- a string matching px, and applies the function produced
-- by pf to the value produced by px
    P p <*> P q = P (concatMap q' . p)
        where q' (f,s) = map (\(x,s') -> (f x, s')) (q s)
-- implicitly defined
-- p *> q - reads a string matching p and a string matching q, produces
-- the value produced by q
-- p <* q - reads a string matching p and a string matching q, produces
-- the value produced by p
-- px <**> pf - reads strings matching px and pf, produces the function
-- returned by pf applied to the value produced by px
instance Alternative (Parser s) where
-- A parser that always fails
    empty = P (\_ -> [])
-- p <|> q represents a choice between p or q
    P p <|> P q = P (\s -> p s ++ q s)
-- parsers for binary operators
--
-- given a parser pOp that recognizes a binary operator (producing a function)
-- and a parser pA that recognizes an operand (producing an argument), we
-- can parse a binary operator like so:
--
-- pExpr = pA <**> pOp <*> pExpr
-- <|> pA
--
-- This will recognize zero or more operands separated by operators, but it
-- will produce a chain of function applications that groups to the right.
-- Additionally, the final operator be parsed twice, as the first choice
-- will fail.
--
-- The functions below allow us to define parsers for binary operators that
-- can group to the left or the right, and avoid backtracking.
assocl :: Parser s (a -> a -> a) -> Parser s a -> Parser s a
assocl pop pa = pa <**> ptail
    where
    ptail = (\f b k a -> k (f a b)) <$> pop <*> pa <*> ptail
        <|> pure id
assocr :: Parser s (a -> a -> a) -> Parser s a -> Parser s a
assocr pop pa = pa <**> ptail
    where
    ptail = (\f b k a -> f a (k b)) <$> pop <*> pa <*> ptail
        <|> pure id
-- The most natural way to handle postfix operators is left recursive, which
-- would lead to a non-terminating parser if written naively. The definition
-- below parses postfix operators as a sequence trailing their operand,
-- producing a function that composes the operators.
postfix :: Parser s (a -> a) -> Parser s a -> Parser s a
postfix pop pa = pa <**> pops
    where
    pops = flip (.) <$> pop <*> pops 
        <|> pure id
-- example of use:
--
-- pPlus = (+) <$ pSym '+'
-- pMinus = (-) <$ pSym '-'
-- pMult = (*) <$ pSym '*'
-- pDiv = div <$ pSym '/'
-- pCarat = (^) <$ pSym '^'
--
-- pExpr = assocl (pPlus <|> pMinus) pTerm
-- pTerm = assocl (pMult <|> pDiv) pFact
-- pFact = assocr pCarat pExpo
-- pExpo = pInteger
-- <|> pSym '(' *> pExpr <* pSym ')'
-- data Expr = Var Char | Add Expr Expr | Sub Expr Expr deriving (Show)
--
-- pExpr = assocl pOp pVar
-- where
-- pVar = Var <$> pWhere (\c -> c /= '+' && c /= '-')
--
-- pOp = Add <$ pSym '+'
-- <|> Sub <$ pSym '-'