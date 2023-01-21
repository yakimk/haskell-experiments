--https://www.hackerrank.com/challenges/electronics-shop/problem    

main :: IO()
main = do
    [b, _ , _] <- getList
    k <- getList
    u <- getList
    print $ solve b k u 


solve :: Int -> [Int] -> [Int] -> Int
solve n k u
    | prices /= []  = maximum prices
    | otherwise = -1
    where
        prices = [ p + q | p <- k, q <- u, p+q <= n]

getList :: IO[Int]
getList = do map read . words <$> getLine