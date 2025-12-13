module DayThree where

calculate :: IO ()
calculate = do
    contents <- readFile "3.txt"
    putStrLn ("Part one: " ++ show (maxJoltages (lines contents)))

maxJoltages :: [String] -> Int
maxJoltages [] = 0
maxJoltages (joltage:joltages) = (maxJoltage '0' '0' joltage) + (maxJoltages joltages)

maxJoltage :: Char -> Char -> [Char] -> Int
maxJoltage _ _ [] = 0
maxJoltage maxStart maxEnd (joltage:[]) = 
    combineDigits maxStart (max maxEnd joltage)

maxJoltage maxStart maxEnd (joltage:joltages)
    | joltage > maxStart = maxJoltage joltage '0' joltages
    | joltage > maxEnd = maxJoltage maxStart joltage joltages
    | otherwise = maxJoltage maxStart maxEnd joltages

combineDigits :: Char -> Char -> Int
combineDigits first second = read ([first] ++ [second])