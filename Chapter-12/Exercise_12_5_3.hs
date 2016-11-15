{-# OPTIONS_GHC -Wall #-}
module Exercise_12_5_3 where

newtype Curry a b = C (a -> b)

instance Functor (Curry a) where
    fmap g (C h) = C (g . h)

instance Applicative (Curry a) where
    pure x = C (\_ -> x)
    C g <*> C h = C (\x -> (g x) (h x))