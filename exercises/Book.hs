-- https://www.hackerrank.com/challenges/drawing-book/problem

main :: IO()
main = interact $  show . solve . map read . words

solve :: [Int] -> Int
solve list = min res $ (n-p+m)  `div` 2
    where 
        n = head list
        p = last list
        res =  p `div` 2
        m = p `mod` 2
