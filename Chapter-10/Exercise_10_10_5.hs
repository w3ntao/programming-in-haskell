{-# OPTIONS_GHC -Wall #-}
module Exercise_10_10_5 where

import Exercise_10_10_4 hiding (readNum, adder)

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getInt
           addList <- sequence [getInt | _ <- [0..n-1]]
           putStr $ "The total is " ++ show (sum addList) ++ "\n"