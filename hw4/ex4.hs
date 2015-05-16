{-# OPTIONS_GHC -Wall #-}

import Data.List (delete)

filterList :: (Eq a) => [a] -> [a] -> [a]

{-filterList a = foldl (.) (\x -> x) (map (\x -> filter (/=x)) $ a)-}
filterList a b = foldl (flip delete) b a

sieveSundaram :: Integer -> [Integer]

sieveSundaram n = map (\x -> 2*x+1) $ filterList (f n) [1..n]
    where f x = [i+j+2*i*j | i <- [1..x `div` 2], j <- [i..x `div` 2]]
