{-# OPTIONS_GHC -Wall #-}
module Exercise_10_10_1 where

import Prelude hiding (putStr)

putStr :: String -> IO ()
putStr xs = sequence_ [putChar x | x <- xs]