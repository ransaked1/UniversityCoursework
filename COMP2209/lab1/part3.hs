prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

main = do
    putStrLn $ "2 * 2: " ++ show (prod [2,2])
    putStrLn $ "2 * 4 * 5: " ++ show (prod [2,4,5])
    putStrLn $ "10 * 2 * 0: " ++ show (prod [10,2,0])
    putStrLn $ "Empty: " ++ show (prod [])