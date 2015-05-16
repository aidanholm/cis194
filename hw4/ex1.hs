{-# OPTIONS_GHC -Wall #-}

fun1' :: [Integer] -> Integer

fun1' = foldl (*) 1 . map (\x -> x-2) . filter even

fun2' :: Integer -> Integer

fun2' x = sum . filter even . takeWhile (\n -> n>1) $ iterate (\n -> if even n then n `div` 2 else 3*n+1) x
