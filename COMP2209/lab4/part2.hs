--Using foldl define a function that takes a list of digits as a representation of a decimal number
--and converts it in to an integer representing that number.
dec2Int :: [Int] -> Int
dec2Int = foldl (\x y -> x*10+y) 0

main = do
    putStrLn $ "dec2Int [1,2,3,4]: " ++ show (dec2Int [1,2,3,4])
