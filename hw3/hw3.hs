{-# OPTIONS_GHC -Wall #-}
module Golf where

{-Exercise 1-}

skips :: [a] -> [[a]]

{- Very ugly solution ... -}
skips l = map (\x -> g l x) [0..length l - 1]
    where g [] _ = []
          g ll x = (h $ drop x ll) ++ (g (t $ drop x ll) x)
          h [] = []
          h (x:_) = [x]
          t [] = []
          t (_:xs) = xs

{-Exercise 2-}

localMaxima :: [Integer] -> [Integer]
localMaxima a = map (\x -> x !! 1) . filter n . map (take 3) . take (length a-2) $ iterate tail a
    where n x = x !! 0 < x !! 1 && x !! 1 > x !! 2

{-Exercise 3-}

histogram :: [Integer] -> String
histogram l = foldl1 (++) $ (map (\n -> row n l) $ reverse [0..(foldl1 max $ h l)-1]) ++ [s]
        where h x = [ c | n <- [0..9], let c = (toInteger . length . filter (==n)) x]
              row y r = (map (\v -> if v>y then '*' else ' ') $ h r) ++ ['\n']
              s = "==========\n0123456789\n"
