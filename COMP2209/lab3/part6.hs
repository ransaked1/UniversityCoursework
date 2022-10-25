find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

main = do
    putStrLn $ "Positions: " ++ show (positions 0 [0,1,0,3,4,5,0])