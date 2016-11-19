{-# OPTIONS_GHC -Wall #-}
module Exercise_14_5_1 where

newtype DoubleMonoid a b = D (a, b)

instance (Monoid a, Monoid b) => Monoid (DoubleMonoid a b) where
    mempty = D (mempty, mempty)
    D (x0, y0) `mappend` D (x1, y1) = D (x0 `mappend` x1, y0 `mappend` y1)