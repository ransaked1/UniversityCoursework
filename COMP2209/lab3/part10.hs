--the Minimum Hamming Distance between the strings is the least Hamming Distance between any pair of strings in the set.
hammingDistance :: Eq a => Int -> [a] -> [a] -> Int
hammingDistance acc [] [] = acc
hammingDistance acc (x:xs) (y:ys) = if x /= y then hammingDistance (acc+1) xs ys else hammingDistance acc xs ys

minHamming :: Eq a => [[a]] -> Int
minHamming strs = minimum [hammingDistance 0 str1 str2 | str1 <- strs, str2 <- strs, str1/=str2]

main = do
    putStrLn $ "Min Hamming: " ++ show (minHamming ["haskell","painful","hasketl"])