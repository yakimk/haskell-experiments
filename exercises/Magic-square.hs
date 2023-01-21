-- https://www.hackerrank.com/challenges/magic-square-forming/problem

import Data.List

type MS = [[Int]]

pp :: MS -> IO()
pp = putStrLn . unlines . map (unwords . map show) 



ref :: MS
ref = [[7, 6, 5],
    [7, 2, 8],
    [5, 3, 4]]

magic :: MS
magic = [[8,1,6],
        [3,5,7],
        [4,9,2]]

rotate :: MS -> MS
rotate = map reverse . transpose

refl :: MS -> MS
refl = transpose . rotate


transforms ::  MS -> [MS]
transforms ms = [ applyN m refl $ applyN n rotate ms | n <- [0..3], m <- [0,1]]

listDif :: [Int] -> [Int] -> Int
listDif [] [] = 0
listDif (k:l) (s:m) = abs(k-s) + listDif l m 

cost :: MS -> MS -> Int
cost [] [] = 0
cost (x:xs) (y:ys) = listDif x y + cost xs ys

solve :: MS -> [Int]
solve ms = [cost ms ref| ref <- transforms magic]

applyN :: Read a => Int -> (a -> a) -> a -> a
applyN 0 f x = x
applyN 1 f x = f x
applyN n f x = applyN (n-1) f x

getList :: IO[Int]
getList = map read . words <$> getLine

main :: IO()
main = do
    n <- getList
    m <- getList
    k <- getList
    print $ solve [n,m,k]