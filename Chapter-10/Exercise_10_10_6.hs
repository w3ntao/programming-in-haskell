{-# OPTIONS_GHC -Wall #-}
module Exercise_10_10_6 where

import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = readLine' ""

readLine' :: [Char] -> IO [Char]
readLine' xs = do x <- getCh
                  case x of '\n' -> return xs
                            '\DEL' -> if null xs
                                          then readLine' ""
                                          else do putStr "\b \b"
                                                  readLine' (init xs)
                            _ -> do putChar x
                                    readLine' (xs ++ [x])






