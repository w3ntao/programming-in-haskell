{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_4 where

import Prelude hiding (replicate)

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [0..(n-1)]]