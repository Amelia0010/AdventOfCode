module DayOne where

calculate :: IO ()
calculate = do
    contents <- readFile "1.txt"
    putStrLn contents