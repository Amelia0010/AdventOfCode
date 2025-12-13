module DayOne where

calculate :: IO ()
calculate = do
    contents <- readFile "1.txt"
    putStrLn (show (calculateZeroCount (lines contents) 50))

calculateZeroCount :: [String] -> Int -> Int
calculateZeroCount [] _ = 0
calculateZeroCount operations dial
    | nextDial == 0 = recursiveZeros + 1
    | otherwise = recursiveZeros
    where
        operation = head operations
        nextDial = calculateNextDial operation dial
        recursiveZeros = calculateZeroCount (tail operations) nextDial


calculateNextDial :: String -> Int -> Int
calculateNextDial operation dial =
    (dial + trueAngle) `mod` 100 
    where 
        direction = head operation
        angle = read (tail operation)
        trueAngle
            | direction == 'R' = angle
            | otherwise = -angle
