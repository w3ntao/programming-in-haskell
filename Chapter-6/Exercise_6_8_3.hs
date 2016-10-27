{-# OPTIONS_GHC -Wall #-}
module Exercise_6_8_3 where

import Prelude hiding ((^))

(^) :: (Num a, Integral b) => a -> b -> a
_ ^ 0 = 1
x ^ n = x * x^(n-1)