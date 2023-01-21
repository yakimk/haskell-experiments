-- https://www.hackerrank.com/challenges/magic-square-forming/problem
import Data.List

type MS = [[Int]]

pp :: MS -> IO()
pp = putStrLn . unlines . map (unwords . map show) 

applyN :: Read a => Int -> (a -> a) -> a -> a
applyN 0 f x = x
applyN n f x = applyN (n-1) f x

magic :: MS
magic = [[8,1,6],
        [3,5,7],
        [4,9,2]]

rotate :: MS -> MS
rotate = map reverse . transpose

refl :: MS -> MS
refl = transpose . rotate

main :: IO()
main = undefined

transforms ::  MS -> [MS]
transforms ms = [ applyN m refl $ applyN n rotate ms | n <- [0..3], m <- [0,1]]

listDif :: [Int] -> [Int] -> Int
listDif l [] = sum $ map abs l
listDif [] m = sum $ map abs m
listDif (k:l) (s:m) = abs(k-s) + listDif l m 

cost :: MS -> MS -> Int
cost = sum $ map listDif