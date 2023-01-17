{- https://www.hackerrank.com/challenges/between-two-sets/problem-}


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
solve ns ms =length 
    $ filter (\x -> mgcd `mod` x ==0)
    $ takeWhile (<= mgcd) 
    $ map (nlcm * ) [1..]

    where 
        nlcm = fold lcm ns
        mgcd = fold gcd ms

fold :: (a -> a -> a) -> [a] -> a
fold f [] = error "Empty list in fold."
fold f [x] = x
fold f (x:xs) = f x $ fold f xs