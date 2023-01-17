import Data.List

solve :: [Int] -> Int
solve = sum . map (`div` 2) . map length . group . sort

main :: IO()
main = interact $ show . solve . map read . tail .  words

