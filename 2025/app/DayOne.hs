module DayOne where

run :: IO ()
run = do
    contents <- readFile "1.txt"
    putStrLn contents