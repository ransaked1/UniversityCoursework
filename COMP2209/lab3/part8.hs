--Use pattern matching and recursion to write a function shorter that accepts two lists and
--returns true if the first list is strictly shorter than the second list and false otherwise.
shorter :: [a] -> [a] -> Bool
shorter [] [a] = True
shorter [a] [] = False
shorter [] [] = False
shorter (x:xs) (y:ys) = shorter xs ys

main = do
    putStrLn $ "Shorter True: " ++ show (shorter [1,2,3,4] [1,2,3,4,5])
    putStrLn $ "Shorter False: " ++ show (shorter [1,2,3,4] [1,2,3,4])
    putStrLn $ "Shorter False: " ++ show (shorter [1,2,3,4,5] [1,2,3,4])