module DayOne where

dialStart :: Int
dialStart = 50

calculate :: IO ()
calculate = do
    contents <- readFile "1.txt"
    putStrLn ("Part one: " ++ show (calculateStopAtZeroCount (lines contents) dialStart))
    putStrLn ("Part two: " ++ show (calculatePassByZeroCount (lines contents) dialStart))

calculateStopAtZeroCount :: [String] -> Int -> Int
calculateStopAtZeroCount [] _ = 0
calculateStopAtZeroCount (operation:operations) dial
    | nextDial == 0 = 1 + recursiveZeros
    | otherwise = recursiveZeros
    where
        move = parseMove operation
        nextDial = calculateNextDial dial move
        recursiveZeros = calculateStopAtZeroCount operations nextDial

calculatePassByZeroCount :: [String] -> Int -> Int
calculatePassByZeroCount [] _ = 0
calculatePassByZeroCount (operation:operations) dial =
    passCount + (calculatePassByZeroCount operations nextDial)
    where
        nextDial = calculateNextDial dial move
        move = parseMove operation
        passCount = passByZeroCount dial move

calculateNextDial :: Int -> Int -> Int
calculateNextDial dial move =
    (dial + move) `mod` 100 

parseMove :: String -> Int
parseMove [] = 0
parseMove (direction:angleStr)
    | direction == 'R' = angle
    | otherwise = -angle
    where 
        angle = read angleStr

passByZeroCount :: Int -> Int -> Int
passByZeroCount dial move
    | dial == 0 = abs(move) `div` 100
    | nextDial == 0 = 1
    | move > 0 = nextDial `div` 100
    | otherwise = abs((nextDial -1) `div` 100)
    where
        nextDial = dial + move