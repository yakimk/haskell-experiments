-- https://www.hackerrank.com/challenges/magic-square-forming/problem

import Data.List

type MS = [[Int]]

pp :: MS -> IO()
pp = putStrLn . unlines . map (unwords . map show) 

ref :: MS
ref = [ [2, 4, 9],
        [6, 8, 1],
        [7, 3, 5]]

magic :: MS
magic = [[8,1,6],
        [3,5,7],
        [4,9,2]]

rotate :: MS -> MS
rotate = map reverse . transpose

refl :: MS -> MS
refl = transpose . rotate


transforms ::  [MS]
transforms = take 4 (iterate rotate magic) ++ take 4 (iterate rotate $ refl magic)

listDif :: [Int] -> [Int] -> Int
listDif [] [] = 0
listDif (k:l) (s:m) = abs(k-s) + listDif l m 

cost :: MS -> MS -> Int
cost [] [] = 0
cost (x:xs) (y:ys) = listDif x y + cost xs ys

solve :: MS -> Int
solve ms = minimum [cost ms ref| ref <- transforms]

getList :: IO[Int]
getList = map read . words <$> getLine

main :: IO()
main = do
    n <- getList
    m <- getList
    k <- getList
    print $ solve [n,m,k]

same :: [Int] -> Bool
same [] = True
same [x] = True
same (x:xs) = x== head xs && same xs

getLineInt :: IO Int
getLineInt = read <$> getLine


chop :: Int -> [Int] -> [[Int]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

isMagic :: MS -> Bool
isMagic ms = (==1) $ length $ nub $ concat [diags, colls, rows] 
    where 
        rows = map sum ms
        colls = map sum $ transpose ms
        diags = [sum $ zipWith (curry (uncurry (!!))) ms [0 .. ],sum $ zipWith (curry (uncurry (!!))) (rotate ms) [0 .. ]]

genAllSquares :: Int -> Int -> [MS]
genAllSquares n r = filter isMagic $ map (chop r) $ permutations [1..n]