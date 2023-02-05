zipL :: (Integral a, Show a) => ([a],[a]) -> [[a]]
zipL ([],[]) = []
zipL ((x:xs),[]) = [x] : zipL (xs,[])
zipL ([],(y:ys)) = [y] : zipL ([],ys)
zipL ((x:xs),(y:ys)) = [x,y] : zipL (xs,ys)

main = do
    putStrLn $ "zipL ([1,2,3,4],[5,6,7,8]): " ++ show (zipL ([1,2,3,4],[5,6,7,8]))
    putStrLn $ "zipL ([],[]): " ++ show (zipL ([],[]))
    putStrLn $ "zipL ([1,2,3,4],[5,6,7]): " ++ show (zipL ([1,2,3,4],[5,6,7]))
    putStrLn $ "zipL ([1,2,3],[5,6,7,8]): " ++ show (zipL ([1,2,3],[5,6,7,8]))