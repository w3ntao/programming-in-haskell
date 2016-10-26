{-# OPTIONS_GHC -Wall #-}
module Exercise_4_7_3 where

safetail0 :: [a] -> [a]
safetail0 xs = if null xs
                   then []
                   else tail xs

safetail1 :: [a] -> [a]
safetail1 xs | null xs   = []
             | otherwise = tail xs

safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 xs = tail xs