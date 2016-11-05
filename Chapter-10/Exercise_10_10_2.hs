{-# OPTIONS_GHC -Wall #-}
module Exercise_10_10_2 where

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

type Board = [Int]

putBoard :: Board -> IO ()
putBoard b = putIter 1 b
             where putIter _ []       = return ()
                   putIter n (x : xs) = do putRow  n     x
                                           putIter (n+1) xs