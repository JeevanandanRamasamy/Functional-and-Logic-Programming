module HW2 where

-- Types and helper functions
-- --------------------------

data Tree a = Tip | Bin a (Tree a) (Tree a) deriving (Show, Eq)

-- Some sample trees. Feel free to add your own.

test1 =
    Bin 4
        (Bin 1
            Tip
            (Bin 2 Tip Tip))
        (Bin 6
            (Bin 5 Tip Tip)
            (Bin 7
                Tip
                (Bin 8 Tip Tip)))

test2 =
    Bin "hello"
        (Bin "goodbye"
            (Bin "ahoy" Tip Tip)
            (Bin "goo" Tip Tip))
        (Bin "helloooo" Tip Tip)

height :: Tree a -> Integer
height Tip = 0
height (Bin a l r) = 1 + max (height l) (height r)


-- displayTree prints out a tree in a visual format. You may find it helpful for
-- examining the structure and contents of a tree.
--
-- Do not worry if this code seems incomprehensible.

displayTree :: (Show a) => Tree a -> IO ()
displayTree Tip = return ()
displayTree (Bin a l r) = do
    above "  " r
    putStrLn ("+ " ++ show a)
    below "  " l
    where
    above indent Tip = return ()
    above indent (Bin a l r) = do
        above (indent ++ "  ") r
        putStrLn (indent ++ "+ " ++ show a)
        below (indent ++ "| ") l
        putStrLn (indent ++ "| ")

    below indent Tip = return ()
    below indent (Bin a l r) = do
        putStrLn (indent ++ "| ")
        above (indent ++ "| ") r
        putStrLn (indent ++ "+ " ++ show a)
        below (indent ++ "  ") l

-- *HW2> displayTree test1
--       + 8
--       |
--     + 7
--     |
--   + 6
--   | |
--   | + 5
--   |
-- + 4
--   |
--   | + 2
--   | |
--   + 1
-- *HW2> displayTree test2
--   + "helloooo"
--   |
-- + "hello"
--   |
--   | + "goo"
--   | |
--   + "goodbye"
--     |
--     + "ahoy"


-- Assignment
-- ----------

-- 0. (ungraded) emptyTree t returns True if and only if t is an empty tree

emptyTree :: Tree a -> Bool
emptyTree Tip = True
emptyTree _ = False

-- As with HW1, replace the dummy definition with a correct one. E.g.,
--
-- emptyTree Tip = True
-- emptyTree _ = False
--
-- Note that the type signature of emptyTree prevents us from using ==. Note also
-- that we could use height t == 0, but that would be less efficient.


-- 1. sizeTree t returns the number of elements in t. This is the same as the number
-- of Bin nodes.

sizeTree :: Tree a -> Integer
sizeTree Tip = 0
sizeTree (Bin a l r) = 1 + sizeTree l + sizeTree r

-- *HW2> sizeTree test1
-- 7
-- *HW2> sizeTree test2
-- 5

-- 2. mirrorTree t returns the "mirror image" of a tree. That is, a tree whose left
-- subtrees correspond to right subtrees in t, and vice versa.

mirrorTree :: Tree a -> Tree a
mirrorTree Tip = Tip
mirrorTree (Bin a l r) = Bin a (mirrorTree r) (mirrorTree l)

-- *HW2> displayTree (mirrorTree test1)
--   + 1
--   | |
--   | + 2
--   |
-- + 4
--   |
--   | + 5
--   | |
--   + 6
--     |
--     + 7
--       |
--       + 8

-- Note that mirroring a tree twice should return the original tree.
-- Or, mirrorTree . mirrorTree = id


-- 3. trimTree n t truncates a tree such that its height is at most n.
-- If n is greater than or equal to the height of t, then trimTree n t = t.

trimTree :: Integer -> Tree a -> Tree a
trimTree n Tip = Tip
trimTree n (Bin a l r)
    | n > 0     = Bin a (trimTree (n - 1) l) (trimTree (n - 1) r)
    | otherwise = Tip

-- *HW2> displayTree (trimTree 1 test1)
-- + 4
-- *HW2> displayTree (trimTree 3 test1)
--     + 7
--     |
--   + 6
--   | |
--   | + 5
--   |
-- + 4
--   |
--   | + 2
--   | |
--   + 1
-- *HW2> displayTree (trimTree 4 test1)
--       + 8
--       |
--     + 7
--     |
--   + 6
--   | |
--   | + 5
--   |
-- + 4
--   |
--   | + 2
--   | |
--   + 1
-- *HW2> trimTree 4 test1 == test1
-- True

-- 4. insertBST a t inserts a into a binary search tree t. That is, it returns
-- a binary search tree t' that is equal to t except that one Tip has been
-- replaced with Bin a Tip Tip.
--
-- Exception: If a is already present in t, insertBST a t = t.

insertBST :: (Ord a) => a -> Tree a -> Tree a
insertBST a Tip = Bin a Tip Tip
insertBST a (Bin b l r)
    | a < b     = Bin b (insertBST a l) r
    | a > b     = Bin b l (insertBST a r)
    | otherwise = Bin b l r

-- *HW2> displayTree (insertBST 3 test1)
--       + 8
--       |
--     + 7
--     |
--   + 6
--   | |
--   | + 5
--   |
-- + 4
--   |
--   |   + 3
--   |   |
--   | + 2
--   | |
--   + 1
-- *HW2> displayTree (insertBST 2 test1)
--       + 8
--       |
--     + 7
--     |
--   + 6
--   | |
--   | + 5
--   |
-- + 4
--   |
--   | + 2
--   | |
--   + 1