{-# OPTIONS_GHC -Wall #-}
module Exercise_12_5_2 where

newtype Curry a b = C (a -> b)

instance Functor (Curry a) where
    fmap g (C h) = C (g . h)