{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Prelude (Show, print)
import GHC.Base

data Expr a
  = V a
  | One
  | (Expr a) :- (Expr a)
  | (Expr a) :* (Expr a)
  | (Expr a) :+ (Expr a)
  | (Expr a) :/ (Expr a)
  deriving (Show, Eq, Functor)

data Equation a
  = Expr a := Expr a
  deriving (Show)
infixl 6 :=

-- solve :: Equation a -> Var a -> Expr a

class Mul a b c | a b -> c where
  (*) :: a -> b -> c

class Div a b c | a b -> c where
  (/) :: a -> b -> c

instance Eq a => Mul (Equation a) (Expr a) (Equation a) where
  (expr1 := expr2) * var
    = expr1 * var := expr2 * var

instance Eq a => Div (Equation a) (Expr a) (Equation a) where
  (expr1 := expr2) / var
    = expr1 / var := expr2 / var

instance Eq a => Mul (Expr a) (Expr a) (Expr a) where
  (x :- y) * var = (x :- y) :* var
  (x :+ y) * var = (x :+ y) :* var
  (x :/ y) * var
    | var == y = x
    | otherwise = x :* var :/ y
  (x :* y) * var = x :* y :* var

instance Eq a => Div (Expr a) (Expr a) (Expr a) where
  expr / var | expr == var
    = One :/ var
  (x :- y) / var = (x :- y) :/ var
  (x :+ y) / var = (x :+ y) :/ var
  (x :/ y) / var
    | var == x = One :/ y
    | otherwise = (x :/ y) :/ var
  (x :* y) / var
    | var == x = y
    | otherwise = (x :* y) :/ var

main = do
  let (a, b, c, d) = (V "a", V "b", V "c", V "d")
  let eq = a :/ b := c :/ d
  print eq
  print $ eq * b
