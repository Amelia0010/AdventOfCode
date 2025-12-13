module DayOne where

calculate :: IO ()
calculate = do
    contents <- readFile "1.txt"
    putStrLn ("Part one: " ++ show (calculateZeroCount (lines contents) 50))

calculateZeroCount :: [String] -> Int -> Int
calculateZeroCount [] _ = 0
calculateZeroCount (operation:operations) dial
    | nextDial == 0 = 1 + recursiveZeros
    | otherwise = recursiveZeros
    where
        nextDial = calculateNextDial operation dial
        recursiveZeros = calculateZeroCount operations nextDial


calculateNextDial :: String -> Int -> Int
calculateNextDial operation dial =
    (dial + trueAngle) `mod` 100 
    where 
        direction = head operation
        angle = read (tail operation)
        trueAngle
            | direction == 'R' = angle
            | otherwise = -angle
