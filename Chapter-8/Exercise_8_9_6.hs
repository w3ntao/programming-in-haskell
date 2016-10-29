{-# OPTIONS_GHC -Wall #-}
module Exercise_8_9_6 where

import Exercise_8_9_5

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)