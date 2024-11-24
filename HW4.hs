module HW4 where
import DFA
data Symbol = A | B | C deriving (Show, Eq)
symbols = [A,B,C]
-- For this assignment, you will provide DFAs describing several regular
-- languages over the alphabet {A,B,C}. Use the type Symbol, defined above
-- to represent the symbols in the alphabet. You may use any type to
-- represent states.
--
-- Be sure to use validDFA to confirm that your DFA has a transition for
-- every state and symbol.
--
-- Use chartDFA to view the transitions defined by the DFA and recognize
-- to test which strings are in the language.
-- 0. (not graded)
-- The language (ABC)*
dfa0 = DFA
    { alphabet = symbols
    , states = [1,2,3,4]
    , start = 1
    , accept = (== 1)
    , trans = transit
    }
    where
    transit 1 A = 2
    transit 2 B = 3
    transit 3 C = 1
    transit _ _ = 4
-- You may use recognize to test this DFA with several strings.
--
-- *HW4> recognize dfa0 [A,B,C]
-- True
-- *HW4> recognize dfa0 []
-- True
-- *HW4> recognize dfa0 [A,B,C,A,B,C]
-- True
-- *HW4> recognize dfa0 [A,B,B]
-- False
-- 1. All strings containing at least one A.
dfa1 = DFA
    { alphabet = symbols
    , states = [1,2]
    , start = 1
    , accept = (== 2)
    , trans = transit
    }
    where
    transit 1 A = 2
    transit 1 _ = 1
    transit 2 _ = 2
-- 2. All strings containing exactly three symbols.
dfa2 = DFA
    { alphabet = symbols
    , states = [1,2,3,4,5]
    , start = 1
    , accept = (== 4)
    , trans = transit
    }
    where
    transit 1 _ = 2
    transit 2 _ = 3
    transit 3 _ = 4
    transit _ _ = 5
-- 3. All strings containing an even number of C's. (Zero is an even number.)
dfa3 = DFA
    { alphabet = symbols
    , states = [1,2]
    , start = 1
    , accept = (== 1)
    , trans = transit
    }
    where
    transit 1 C = 2
    transit 1 _ = 1
    transit 2 C = 1
    transit 2 _ = 2
-- 4. All strings, except those containing the sequence AB.
dfa4 = DFA
    { alphabet = symbols
    , states = [1,2,3]
    , start = 1
    , accept = (/= 3)
    , trans = transit
    }
    where
    transit 1 A = 2
    transit 1 _ = 1
    transit 2 B = 3
    transit 2 _ = 1
    transit 3 _ = 3