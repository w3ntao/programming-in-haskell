{-# OPTIONS_GHC -Wall #-}
module Exercise_15_9_6 where

approx :: Double
approx = 1.0

delta :: Double
delta = 0.00001

sqroot :: Double -> Double
sqroot n = head . filter (\x -> abs (x*x - n) < delta) $ iterate next approx
           where next a = (a + n/a) / 2