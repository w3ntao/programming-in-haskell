{-# OPTIONS_GHC -Wall #-}
module Exercise_12_5_7 where

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
    fmap _ (Val x)   = Val x
    fmap g (Var x)   = Var (g x)
    fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
    pure x = Var x
    _       <*> Val x   = Val x
    Val x   <*> _       = Val x
    Var f   <*> Var x   = Var (f x)
    Var f   <*> Add x y = Add (fmap f x) (fmap f y)
    Add f g <*> x       = Add (f <*> x)  (g <*> x)

instance Monad Expr where
    return = pure
    Val x   >>= _ = Val x
    Var x   >>= f = f x
    Add x y >>= f = Add (x >>= f) (y >>= f)