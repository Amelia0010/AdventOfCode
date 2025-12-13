module DayThree where

calculate :: IO ()
calculate = do
    contents <- readFile "3.txt"
    putStrLn ("Part one: " ++ show (maxJoltages (lines contents)))

maxJoltages :: [String] -> Int
maxJoltages [] = 0
maxJoltages (joltage:joltages) = (maxJoltage 0 0 joltage) + (maxJoltages joltages)

maxJoltage :: Int -> Int -> [Char] -> Int
maxJoltage _ _ [] = 0
maxJoltage maxStart maxEnd (joltageStr:[]) = 
    combineDigits maxStart (max maxEnd joltage)
    where
        joltage = read [joltageStr]

maxJoltage maxStart maxEnd (joltageStr:joltages)
    | joltage > maxStart = maxJoltage joltage 0 joltages
    | joltage > maxEnd = maxJoltage maxStart joltage joltages
    | otherwise = maxJoltage maxStart maxEnd joltages
    where
        joltage = read [joltageStr]

combineDigits :: Int -> Int -> Int
combineDigits first second = read ((show first) ++ (show second))