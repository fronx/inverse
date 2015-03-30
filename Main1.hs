{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Prelude (Show, show, print, Num)
import GHC.Base

data Expr a n
  = V a -- variable
  | N n -- numeric value
  | (Expr a n) :- (Expr a n)
  | (Expr a n) :* (Expr a n)
  | (Expr a n) :+ (Expr a n)
  | (Expr a n) :/ (Expr a n)
  deriving (Eq)

data Equation a n = Expr a n := Expr a n
infixl 6 :=

-- solve :: Equation a -> Var a -> Expr a

class Mul a b c | a b -> c where
  (*) :: a -> b -> c

class Div a b c | a b -> c where
  (/) :: a -> b -> c

instance (Eq a, Eq n, Num n) => Mul (Equation a n) (Expr a n) (Equation a n) where
  (expr1 := expr2) * var
    = expr1 * var := expr2 * var

instance (Eq a, Eq n, Num n) => Div (Equation a n) (Expr a n) (Equation a n) where
  (expr1 := expr2) / var
    = expr1 / var := expr2 / var

instance (Eq a, Eq n, Num n) => Mul (Expr a n) (Expr a n) (Expr a n) where
  (x :- y) * var = (x :- y) :* var
  (x :+ y) * var = (x :+ y) :* var
  (x :/ y) * var
    | var == y = x
    | otherwise = x :* var :/ y
  (x :* y) * var = x :* y :* var

instance (Eq a, Eq n, Num n) => Div (Expr a n) (Expr a n) (Expr a n) where
  expr / var | expr == var
    = N 1 :/ var
  (x :- y) / var = (x :- y) :/ var
  (x :+ y) / var = (x :+ y) :/ var
  (x :/ y) / var
    | var == x = N 1 :/ y
    | otherwise = (x :/ y) :/ var
  (x :* y) / var
    | var == x = y
    | otherwise = (x :* y) :/ var

instance (Show a, Show n) => Show (Expr a n) where
  show (N n) = show n
  show (V v) = show v
  show (e :- e') = show e ++ " - " ++ show e'
  show (e :+ e') = show e ++ " + " ++ show e'
  show (e :/ e') = show e ++ " / " ++ show e'
  show (e :* e') = show e ++ " * " ++ show e'

instance (Show a, Show n) => Show (Equation a n) where
  show (e := e') = show e ++ " = " ++ show e'

-- (<--) :: Expr a n -> a -> n -> Expr a n

main = do
  let (a, b, c, d) = (V "a", V "b", V "c", V "d")

  let eq = a :/ b := c :/ d
      eq :: Equation String Double

  print eq
  print $ eq * b
  print $ eq / a * b

  --print $ eq :<--
