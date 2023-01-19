-- https://www.hackerrank.com/challenges/drawing-book/problem

main :: IO()
main = interact $  show . solve . map read . words

solve :: [Int] -> Int
solve list = min res resn
    where 
        n = head list
        p = last list
        res =  p `div` 2
        resn = (n `div` 2) - res 
