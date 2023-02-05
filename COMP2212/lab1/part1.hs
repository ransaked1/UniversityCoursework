zipL :: (Integral a, Show a) => ([a],[a]) -> [[a]]
zipL (xs,ys) | length xs == length ys = [[x,y] | (x,y) <- zip xs ys]
             | otherwise = []

unzipL :: (Integral a, Show a) => [[a]] -> ([a],[a])
unzipL pairs = ([head p | p <- pairs], [(head . reverse) p | p <- pairs])

main = do
    putStrLn $ "zipL ([1,2,3,4],[5,6,7,8]): " ++ show (zipL ([1,2,3,4],[5,6,7,8]))
    putStrLn $ "zipL ([],[]): " ++ show (zipL ([],[]))
    putStrLn $ "zipL ([1,2,3,4],[5,6,7]): " ++ show (zipL ([1,2,3,4],[5,6,7]))
    putStrLn $ "zipL ([1,2,3],[5,6,7,8]): " ++ show (zipL ([1,2,3],[5,6,7,8]))

    putStrLn $ "unzipL [[1,5],[2,6],[3,7],[4,8]]: " ++ show (unzipL [[1,5],[2,6],[3,7],[4,8]])
    putStrLn $ "unzipL []: " ++ show (unzipL [])

    putStrLn $ "unzipL (zipL ([1,2,3,4],[5,6,7,8])): " ++ show (unzipL (zipL ([1,2,3,4],[5,6,7,8])))