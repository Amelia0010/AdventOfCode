module Main where
import DayOne

runDay :: String -> IO ()
runDay "1" = DayOne.calculate
runDay day = putStrLn ("Invalid Day: " ++ day)

main :: IO ()
main = do
    putStrLn "Please choose a day from [1]"
    day <- getLine
    runDay day
    
