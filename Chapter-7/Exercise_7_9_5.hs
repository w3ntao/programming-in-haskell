{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_5 where

import Prelude hiding (curry)

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y