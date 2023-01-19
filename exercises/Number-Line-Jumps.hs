-- https://www.hackerrank.com/challenges/kangaroo/problem

main :: IO ()
main = interact $ solve . map read . words 

solve :: [Int] ->String
solve [x1,v1,x2,v2]
    | modular == 0 && v2<v1 =  "YES"
    | otherwise = "NO"
    where  
        modular = (x2-x1) `mod` (v1-v2)