{-# OPTIONS_GHC -Wall #-}
module Exercise_10_10_3 where

import Exercise_10_10_2 hiding (putBoard)

putBoard :: Board -> IO ()
putBoard xs = sequence_ [putRow n x | (n, x) <- zip [1..] xs]