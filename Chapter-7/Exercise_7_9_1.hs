{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_1 where

highOrder :: (a -> b) -> (a -> Bool) -> [a] -> [b]
highOrder f p = map f . filter p