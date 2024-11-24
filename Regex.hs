module Regex where
data RE symbol 
    = RSym symbol -- match a single symbol
    | REmpty -- match the empty string
    | RNone -- match no strings
    | RChoice (RE symbol) (RE symbol) -- match either regex
    | RCompos (RE symbol) (RE symbol) -- match two regexes in sequence
    | RMany (RE symbol) -- match the regex zero or more times
    | RSome (RE symbol) -- match the regex one or more times
    deriving (Show, Eq)
-- We will use Haskell's built-in lists to represent strings (in the formal
-- language sense).
type Str symbol = [symbol]
-- Produces a list containing
generate :: RE symbol -> [Str symbol]
generate (RSym s) = [[s]]
generate REmpty = [[]]
generate RNone = []
generate (RChoice a b) = interleave (generate a) (generate b)
generate (RCompos a b) = fairConcatMap (\xs -> map (xs ++) (generate b)) (generate a)
generate (RMany a) = [] : generate (RSome a)
generate (RSome a) = generate (RCompos a (RMany a))
-- cs = interleave as bs ensures that every element of as and bs will occur at
-- some finite index of cs. Contrast this with ++, where as ++ bs will never
-- contain elements of bs if as is infinite.
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys = ys
-- A version of concatMap that uses interleave instead of ++
fairConcatMap :: (a -> [b]) -> [a] -> [b]
fairConcatMap f (x:xs) = interleave (f x) (fairConcatMap f xs)
fairConcatMap f [] = []
-- alternative definition:
-- fairConcatMap f = foldr (interleave . f) []
--
-- This is the version of match described in the lecture slides.
-- Given a regular expression and a string, it determines whether the string
-- is part of the language described by the regular expression.
match :: (Eq sym) => RE sym -> Str sym -> Bool
match (RSym c) (s:ss) = c == s && null ss
match (RSym _) [] = False
match REmpty str = null str
match RNone _ = False
match (RChoice a b) str = match a str || match b str
match (RCompos a b) str = any (\(u,v) -> match a u && match b v) (splits str)
match (RMany a) [] = True
match (RMany a) (s:ss) = any (\(u,v) -> match a (s:u) && match (RMany a) v) (splits ss)
match (RSome a) str = any (\(u,v) -> match a u && match (RMany a) v) (splits str)
-- return every way to divide a Str into two sublists
splits :: Str a -> [(Str a, Str a)]
splits [] = [([],[])]
splits (x:xs) = ([],x:xs) : [ (x:u,v) | (u,v) <- splits xs ]
--
-- A variation on match that is potentially more efficient.
match' :: (Eq symbol) => RE symbol -> Str symbol -> Bool
match' r = any null . suffixes True r
-- suffixes determines whether the prefix of a string is in the language of the
-- specified regular expression, returning the unused suffix of the string.
-- If more than one prefix is part of the language, suffixes returns all possible
-- suffixes.
--
-- The first parameter indicates whether the prefix can be an empty string. It
-- is used to prevent infinite recursion when the regular expression is RMany.
suffixes :: (Eq symbol) => Bool -> RE symbol -> Str symbol -> [Str symbol]
suffixes _ (RSym s) (a:as) | s == a = [as]
suffixes True REmpty s = [s]
suffixes e (RChoice a b) s = suffixes e a s ++ suffixes e b s
suffixes True (RCompos a b) s = concatMap (suffixes True b) (suffixes True a s)
suffixes False (RCompos a b) s =
    (if null (suffixes True a []) then [] else suffixes False b s) ++
    concatMap (suffixes True b) (suffixes False a s)
suffixes False (RMany a) s = concatMap (suffixes True (RMany a)) (suffixes False a s)
suffixes True (RMany a) s = s : concatMap (suffixes True (RMany a)) (suffixes False a s)
suffixes e (RSome a) s = suffixes e (RCompos a (RMany a)) s
suffixes _ _ _ = []
--
-- This is a more restricted version of generate which ensures that strings are
-- given in ascending order of length, sorted lexicographically.
--
-- Note: Because the strings are given in increasing length, it is always
-- possible to determine whether a string is in the list, because we can
-- stop looking once we obtain a string longer than the one we are checking.
generate' :: (Ord symbol) => RE symbol -> [Str symbol]
generate' = concat . lgenerate
-- generate groups of strings in some language, where each group contains all
-- strings of a particular length.
--
-- e.g., [[""],["a","b"],["aa","ab","ba",bb",], ...]
lgenerate :: (Ord symbol) => RE symbol -> [[Str symbol]]
lgenerate RNone = []
lgenerate REmpty = [[[]]]
lgenerate (RSym s) = [[], [[s]]]
lgenerate (RChoice a b) = lmerge (lgenerate a) (lgenerate b)
lgenerate (RCompos a b) = lcomb (lgenerate a) (lgenerate b)
lgenerate (RMany a) = lmany (lgenerate a)
lgenerate (RSome a) = lsome (lgenerate a)
lmerge :: (Ord symbol) => [[Str symbol]] -> [[Str symbol]] -> [[Str symbol]]
lmerge (xs:xss) (ys:yss) = merge xs ys : lmerge xss yss
lmerge xss [] = xss
lmerge [] yss = yss
merge :: (Ord symbol) => [Str symbol] -> [Str symbol] -> [Str symbol]
merge (x:xs) (y:ys) =
    case compare x y of
        LT -> x : merge xs (y:ys)
        EQ -> x : merge xs ys
        GT -> y : merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys
lcomb :: (Ord symbol) => [[Str symbol]] -> [[Str symbol]] -> [[Str symbol]]
lcomb xss yss = map merges . stripe $ map (\xs -> map (comb xs) yss) xss
lmany :: (Ord symbol) => [[Str symbol]] -> [[Str symbol]]
lmany [] = [[[]]]
lmany (_:xss) = [[]] : yss
    where
    yss = map merges . stripe $ map (\xs -> xs : map (comb xs) yss) xss
lsome [] = []
lsome (xs:xss) = xs : yss
    where
    yss = map merges . stripe $ map (\xs -> xs : map (comb xs) yss) xss
comb xs ys = concatMap (\x -> map (x++) ys) xs
merges xss = foldr merge [] xss
stripe [] = []
stripe ([]:xss) = [] : stripe xss
stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)
zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys
zipCons [] ys = ys
zipCons xs [] = map (:[]) xs