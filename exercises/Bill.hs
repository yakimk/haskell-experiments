getLine' :: IO Int
getLine' = do
    l <- getLine
    return $ last . map read . words l  

main :: IO()
main = do
    k  <- getLine'
    food <- getList
    price <- getList
    putStrLn $ maybe "Bon Appetit" show $ solve ((k:food) ++ price)

getList :: IO[Int]
getList = do
    l <- getLine
    return $ map read . words l

remove :: Read a => Int -> [a] -> [a]
remove x [] = []
remove x xs  = left ++ tail right
    where
        (left, right) = splitAt x xs 

solve :: [Int] -> Maybe Int
solve xs
    | final /= 0 = Just final
    | otherwise = Nothing
    where
        list = init $ tail xs
        n = head xs
        res  = sum $ remove n list
        charged = last xs
        final = charged - (res `div` 2)