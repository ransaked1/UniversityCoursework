halve :: [a] -> ([a],[a])
halve xs | even (length xs) = splitAt (((length xs) + 1) `div` 2) xs

main = do
    putStrLn $ "halve even [1,2,3,4,5,6] : " ++ show (halve [1,2,3,4,5,6])