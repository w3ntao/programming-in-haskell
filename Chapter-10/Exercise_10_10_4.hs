{-# OPTIONS_GHC -Wall #-}
module Exercise_10_10_4 where

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getInt
           addSum <- readNum n
           putStr $ "The total is " ++ show addSum ++ "\n"

readNum :: Int -> IO Int
readNum 0 = return 0
readNum n = do x <- getInt
               rest <- readNum (n-1)
               return (x + rest)

getInt :: IO Int
getInt = do x <- getLine
            return ((read x)::Int)