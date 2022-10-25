--Define a function to calculate the scalar product of two lists using a list comprehension.
scalars :: [Int] -> [Int] -> [Int]
scalars xs ys = [ x' * y' | (x', y') <- (zip xs ys)]

main = do
    putStrLn $ "Scalars: " ++ show (scalars [1,2,3,4] [2,3,4,5])