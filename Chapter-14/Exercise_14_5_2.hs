{-# OPTIONS_GHC -Wall #-}
module Exercise_14_5_2 where

newtype Curry a b = C (a -> b)

instance (Monoid b) => Monoid (Curry a b) where
    mempty = C (\_ -> mempty)
    C f `mappend` C g = C (\x -> f x `mappend` g x)