{-# OPTIONS_GHC -Wall #-}

{-Exercise 2-}

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving(Show, Eq)

foldTree :: [a] -> Tree a
foldTree = 
