module Main where
import DayOne
import DayThree

runDay :: String -> IO ()
runDay "1" = DayOne.calculate
runDay "3" = DayThree.calculate
runDay day = putStrLn ("Invalid Day: " ++ day)

main :: IO ()
main = do
    putStrLn "Please choose a day from [1, 3]"
    day <- getLine
    runDay day
    
