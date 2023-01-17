main :: IO()
main = interact $ unwords . map show . solve . map read .  tail . words 

solve :: [Int] -> [Int]
solve list =[high, low]
    where
    high = (length . group . map maximum  . tail $ inits list)-1
    low = (length . group . map minimum  . tail $ inits list)-1