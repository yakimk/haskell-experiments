-- https://www.hackerrank.com/challenges/counting-valleys/problem

import           Data.List

parse :: Char -> Int
parse 'D' = -1
parse 'U' = 1

solve :: String -> Int
solve l = length 
    $ filter ( > 1) 
    $ map length 
    $ groupBy (\x y -> x == 0 && y < 0) 
    $ scanl (+) 0 
    $ map parse l

main :: IO()
main = do
    n <- getLine
    s <- getLine
    print $ solve s
    