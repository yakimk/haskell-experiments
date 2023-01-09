-- https://www.hackerrank.com/challenges/between-two-sets/problem

readIntList :: IO [Int]
readIntList = do
    line <- getLine
    return .  map read $ words line

main :: IO ()
main = do
    [n,m] <- readIntList 
    ns <- readIntList
    ms <- readIntList
    print $ solve ns ms

solve :: [Int] -> [Int] -> Int
solve = undefined