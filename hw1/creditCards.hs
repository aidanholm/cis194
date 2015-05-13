{-# OPTIONS_GHC -Wall #-}

{-Exercise 1-}

toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigits n
    | n < 10 = [n]
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev = reverse . toDigits

{-Exercise 2-}

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther = reverse . helper . reverse
    where helper (x:xs:xss) = x : (2*xs) : helper xss
          helper (x:xs) = (x:xs)
          helper [] = []

{-Exercise 3-}

sumDigits :: [Integer] -> Integer

sumDigits = sum . map sum . map toDigits

{-Exercise 4-}

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0
