{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

{-Exercise 1-}

eval :: ExprT -> Integer

eval (ExprT.Lit n) = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

{-Exercise 2-}

evalStr :: String -> Maybe Integer

evalStr = e . parseExp ExprT.Lit ExprT.Add ExprT.Mul
    where e (Just ex) = Just (eval ex)
          e _ = Nothing

{-Exercise 3-}

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT

reify = id

{-Exercise 4-}

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Ord, Show)

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    add (Mod7 a) (Mod7 b) = (Mod7 $ (a+b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = (Mod7 $ (a*b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

{-Exercise 5-}

instance Expr Program where
        lit v = [StackVM.PushI v]
        add a b = a ++ b ++ [StackVM.Add]
        mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s

