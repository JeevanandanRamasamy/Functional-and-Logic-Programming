module HW1 where
-- Instructions: Download this file, replace the placeholder definitions below
-- with correct ones, and then submit the file via Canvas.
--
-- You may load this file into ghci for testing and verification purposes by
-- copying it to a device with ghci installed (such as any iLab machine) and
-- either specifying the file as a command-line argument:
--
-- $ ghci HW1.hs
--
-- or by starting ghci and loading the file using :load
--
-- $ ghci
-- ghci> :load HW1.hs
--
--
-- Type signatures have been given for the definitions as an additional
-- correctness check. Do not change or remove them.
-- 1. Hailstone sequences are produced by repeatedly applying the following
-- mapping to numbers: an even number n maps to n/2 and an odd number m maps
-- to 3m+1.
--
-- The function hailmap applies this transformation to a single integer.
hailmap :: Integer -> Integer
hailmap n 
    | even n    = n `div` 2
    | otherwise = 3 * n + 1
-- ghci> hailmap 7
-- 22
-- ghci> hailmap 8
-- 4
-- Note: Use div rather than / for division, to avoid the need for conversion
-- between numeric types.
--
-- ghci> 15 `div` 2
-- 7
-- Note that the Prelude includes a function even :: Integer -> Bool that you
-- may find helpful in your definition.
--
-- 2. The function hailstone returns a complete hailstone sequence starting
-- with the specified positive integer. The sequence ends when it reaches 1.
hailstone :: Integer -> [Integer]
hailstone n 
    | n <= 1    = [1]
    | otherwise = n : hailstone (hailmap n)
-- ghci> hailstone 1
-- [1]
-- ghci> hailstone 3
-- [3,10,5,16,8,4,2,1]
-- ghci> hailstone 7
-- [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
-- It is a famously unproven conjecture that all hailstone sequences reach 1
-- within a finite number of steps. Fortunately, lazy evaluation means that
-- we don't need to worry.
--
-- 3. med3 a b c returns the median value of the set {a,b,c}.
med3 :: Integer -> Integer -> Integer -> Integer
med3 a b c
    | (a <= b && a >= c) || (a <= c && a >= b) = a
    | (b <= a && b >= c) || (b <= c && b >= a) = b
    | otherwise                                = c
-- ghci> med3 1 8 17
-- 8
-- ghci> med3 25 (-1) 6
-- 6
-- ghci> med3 2 11 2
-- 2
--
-- 4. blur produces a list containing the means of each pair of numbers in a
-- list. That is, the average of the first and second, the average of the
-- second and third, the average of the third and fourth, and so forth.
--
-- It returns an empty list if the input has fewer than two elements.
blur :: [Double] -> [Double]
blur (a:b:bs) = (a + b) / 2 : blur (b:bs)
blur _        = []
-- ghci> blur [1, 3, 5, 7]
-- [2.0,4.0,6.0]
-- ghci> blur [7, 8, 2.5, 16, 45, 45, 0]
-- [7.5,5.25,9.25,30.5,45.0,22.5]
-- ghci> blur []
-- []
-- ghci> blur [1]
-- []
-- As this function uses Double, use / rather than div for division.