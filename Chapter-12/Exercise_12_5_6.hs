{-# OPTIONS_GHC -Wall #-}
module Exercise_12_5_6 where

newtype Curry a b = C (a -> b)

instance Functor (Curry a) where
    fmap g (C h) = C (g . h)

instance Applicative (Curry a) where
    pure x = C (\_ -> x)
    C g <*> C h = C (\x -> (g x) (h x))

instance Monad (Curry a) where
    return = pure
    C f >>= g = C h
                where h x = h' x
                            where (C h') = g (f x)