module DFA where
import Data.List
import Control.Monad
data DFA state symbol = DFA
    { alphabet :: [symbol]
    , states :: [state]
    , start :: state -- must be in the list of states
    , trans :: state -> symbol -> state
        -- trans dfa x y must return a value in the list of states, whenever x is in the
        -- list of states and y is in the alphabet
    , accept :: state -> Bool
        -- only meaningful if the state is in the list of states
    }
-- String recognition
-- ------------------
-- Determine whether a string is in the language defined by a DFA
recognize :: DFA state symbol -> [symbol] -> Bool
recognize dfa = accept dfa . foldl' (trans dfa) (start dfa)
-- Returns the sequence of states traversed by the DFA while processing
-- the given string.
path :: DFA state symbol -> [symbol] -> [state]
path dfa = scanl (trans dfa) (start dfa)
-- scanl is similar to foldl, except it returns all the intermediate states
-- DFA manipulation
-- ----------------
-- The complement of a regular language includes exactly those strings
-- that are not part of the original language.
--
-- The complement of a DFA accepts any string that the original rejects.
-- recognize (complemenDFA d) == not . recognize d
complementDFA :: DFA state symbol -> DFA state symbol
complementDFA dfa = DFA
    { alphabet = alphabet dfa
    , states = states dfa
    , start = start dfa
    , trans = trans dfa
    , accept = not . accept dfa
    }
-- Formally, the union and intersection are only defined for DFAs over the same
-- alphabet. We could enforce this by throwing an exception if the alphabets don't
-- match, but instead we will extend the concept. Because our union and intersection
-- work by calling the transition functions of the original DFAs, we will limit our
-- new DFAs to symbols occuring in both alphabets.
productDFA :: (Eq symbol)
    => (Bool -> Bool -> Bool) -- decide whether to accept based on whether each DFA accepted
    -> DFA a symbol
    -> DFA b symbol
    -> DFA (a,b) symbol
productDFA f d1 d2 = DFA
    { alphabet = intersect (alphabet d1) (alphabet d2)
    , states = concatMap (\a -> map (\b -> (a,b)) (states d2)) (states d1)
        -- note: some of these states may be unreachable
    , start = (start d1, start d2)
    , trans = \(a,b) s -> (trans d1 a s, trans d2 b s)
    , accept = \(a,b) -> f (accept d1 a) (accept d2 b)
    }
-- The language for unionDFA a b contains every string in the languages for
-- a and b.
unionDFA :: (Eq symbol) => DFA a symbol -> DFA b symbol -> DFA (a,b) symbol
unionDFA = productDFA (||)
-- The language for intersectDFA a b contains every string in both the
-- languages for a and b.
intersectDFA :: (Eq symbol) => DFA a symbol -> DFA b symbol -> DFA (a,b) symbol
intersectDFA = productDFA (&&)
-- The language for subtractDFA a b contains every string in the language for
-- a that is not also in b.
subtractDFA :: (Eq symbol) => DFA a symbol -> DFA b symbol -> DFA (a,b) symbol
subtractDFA = productDFA (\a b -> a && not b)
-- The language for differenceDFA a b contains every string that is in the
-- language for a or the language for b, but not both.
differenceDFA :: (Eq symbol) => DFA a symbol -> DFA b symbol -> DFA (a,b) symbol
differenceDFA = productDFA (/=)
-- Querying DFAs
-- -------------
-- findOne obtains a string in the language for a DFA, or determines
-- that the language contains no strings.
findOne :: (Eq st) => DFA st sym -> Maybe [sym]
findOne d
    | accept d (start d) = Just []
    | otherwise = either (const Nothing) Just $ seek [] (arcs (start d))
    where
    -- obtain all outgoing transitions from a state
    arcs s = map (\x -> (x, trans d s x)) (alphabet d)
-- Use depth-first search to visit every state reachable from
-- the start state, looking for an accept state
--
-- returns Right path if an accept state is found
-- otherwise returns Left seen_states
--
-- We use seen to ensure that we do not visit a node more than once
    seek seen [] = Left seen
    seek seen ((x,s):xss)
        | accept d s = Right [x]
        | s `elem` seen = seek seen xss
        | otherwise = case seek (s:seen) (arcs s) of
            Right str -> Right (x:str)
            Left seen' -> seek seen' xss
-- nullDFA d is True if the language for d has no strings
nullDFA :: (Eq st) => DFA st sym -> Bool
nullDFA = maybe True (const False) . findOne
-- Check whether two DFAs are defined over the same alphabet
sameAlphabet :: (Eq sym) => DFA st sym -> DFA st' sym -> Bool
sameAlphabet d1 d2 = all (\s -> s `elem` alphabet d2) (alphabet d1) &&
    all (\s -> s `elem` alphabet d1) (alphabet d2)
    -- this definition could be made much more efficient if we
    -- allowed the use of compare (and therefore sorting)
-- Two DFAs describe the same language if their difference contains no
-- strings.
instance (Eq st, Eq sym) => Eq (DFA st sym) where
    a == b = sameAlphabet a b && nullDFA (differenceDFA a b)
-- Validating DFAs
-- ---------------
-- Confirm whether this DFA satisfies the invariants for the type
validDFA :: (Eq st, Eq sym) => DFA st sym -> Bool
validDFA d =
    start d `elem` states d -- starting state is in the list of states
    && all validArcs (states d) -- all transitions lead to states in the list
    && nub (states d) == states d -- list of states has no duplicates
    && nub (alphabet d) == alphabet d -- alphabet has no duplicates
    where
    validArcs s = all (\dst -> dst `elem` states d) (map (trans d s) (alphabet d))
-- Print a transition table for a DFA
chartDFA :: (Show st, Show sym) => DFA st sym -> IO ()
chartDFA d = do
    putStrLn $ "start: " ++ show (start d)
    forM_ (states d) $ \s -> do
        if accept d s
            then putStrLn $ show s ++ " accept"
            else putStrLn $ show s
        forM_ (alphabet d) $ \a ->
            putStrLn $ concat [" >= ", show a, " => ", show (trans d s a)]