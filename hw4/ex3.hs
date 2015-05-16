{-# OPTIONS_GHC -Wall #-}

xor :: [Bool] -> Bool

xor = foldl (\x y -> x /= y) False

map' :: (a -> b) -> [a] -> [b]

map' f = foldr (\a b -> f a : b) []
