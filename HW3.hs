-- Download Regex.hs from Canvas under Files->Code
-- It is also recommended that you download Parsing.hs and RegexP.hs.
-- Using these, you can conform that your answers for part 1 have the correct
-- syntax and check the languages they describe.
module HW3 where
import Regex
import RegexP -- uncomment if you have downloaded Parsing.hs and RegexP.hs
-- I. Describing regular languages
-- -------------------------------
-- In this section, you will define six languages over the alphabet {A,B,C}.
--
-- 0. (Example) Strings where A occurs exactly once
re0 = "(B|C)*A(B|C)*"
-- We can confirm correctness using parseRE from RegexP
--
-- *HW3> parseRE re0
-- Just (RCompos (RMany (RChoice (RSym 'B') (RSym 'C'))) (RCompos (RSym 'A')
-- (RMany (RChoice (RSym 'B') (RSym 'C')))))
--
-- We can observe the strings in the language using generate (or generate'):
--
-- *HW3> take 20 (generate (getRE re0))
-- ["A","BA","AB","CA","AC","BAB","ABB","BBA","ACB","BAC","ABC","CAB","ACC",
-- "BABB","ABBB","CBA","ACBB","BACB","ABCB","CAC"]
-- *HW3> take 20 (generate' (getRE re0))
-- ["A","AB","AC","BA","CA","ABB","ABC","ACB","ACC","BAB","BAC","BBA","BCA",
-- "CAB","CAC","CBA","CCA","ABBB","ABBC","ABCB"]
--
-- We can also test whether specific strings are part of the language using
-- match (or match'):
--
-- *HW3> match' (getRE re0) "BCBCACCC"
-- True
-- *HW3> match' (getRE re0) "BCABCACCC"
-- False
-- 1. Strings containing only A, zero or more times
re1 = "A*"
-- 2. Strings where A occurs at most once.
re2 = "(B|C)*(A|e)(B|C)*"
-- 3. Strings where the substring ABC occurs at least once.
re3 = "(A|B|C)*(ABC)+(A|B|C)*"
-- 4. Strings where A occurs an even number of times.
re4 = "((B|C)*A(B|C)*A(B|C)*)*"
-- 5. Strings where A, B, and C each occur at least once.
re5 = "(A|B|C)*((A+B+C+)|(A+C+B+)|(B+A+C+)|(B+C+A+)|(C+A+B+)|(C+B+A+))(A|B|C)*"
-- 6. Strings that do not contain the sequence AB
re6 = "((A+C)|B|C)*A*"
-- II. Regex analysis
-- ------------------
-- 7. Write matchEmpty, which determines whether the language for a regular
-- expression includes the empty string.
matchEmpty :: RE symbol -> Bool
matchEmpty (RSym _) = False
matchEmpty (REmpty) = True
matchEmpty RNone = False
matchEmpty (RChoice re1 re2) = matchEmpty re1 || matchEmpty re2
matchEmpty (RCompos re1 re2) = matchEmpty re1 && matchEmpty re2
matchEmpty (RMany _) = True
matchEmpty (RSome re) = matchEmpty re
-- Note that the type signature does not restrict symbol to types with
-- equality testing, so we cannot use match or match' to check whether the
-- empty string is allowed.
--
-- matchEmpty r = match r [] -- type error
--
-- Similarly, we cannot use generate' and check whether the first string
-- it provides is the empty string.
--
-- Finally, while generate has the proper type, we cannot use it to write a
-- function that always terminates, as we cannot conclude that the empty
-- string is not present until we have examined every string.
--
-- *HW3> matchEmpty (getRE "e")
-- True
-- *HW3> matchEmpty (getRE "abc*")
-- False
-- *HW3> matchEmpty (getRE "a*")
-- True
-- *HW3> matchEmpty (getRE "0")
-- False
-- *HW3> matchEmpty (getRE "0*")
-- True