{-# OPTIONS_GHC -Wall #-}
module Exercise_9_11_2 where

remove :: Eq a => a -> [a] -> [a]
remove _ []       = []
remove x (y : ys) = if x == y
                        then ys
                        else y : remove x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []       _  = True
isChoice _        [] = False
isChoice (x : xs) ys = if not (elem x ys)
                           then False
                           else isChoice xs (remove x ys)