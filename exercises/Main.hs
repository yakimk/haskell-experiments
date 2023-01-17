getMiddle :: [Char]-> [Char]

getMiddle s
    | odd n = [s !! (n `div` 2)]
    | otherwise  = [s !! ((n `div` 2)-1), s !! (n `div` 2)]
    where n = length s

main = do
    s <- getLine
    putStrLn (getMiddle s)



