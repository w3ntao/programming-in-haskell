{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_9 where

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = altF True
             where altF _     []    = []
                   altF at (x : xs) = (if at then f else g) x : altF (not at) xs