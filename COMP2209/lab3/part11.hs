--Without using any list sorting functions, write a recursive function
--that merges two sorted lists to give a single sorted list.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x >= y then y : merge (x:xs) ys else x : merge xs (y:ys)

--Write a function that sorts a list using the merge sort algorithm.
halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (fst (halve xs))) (mergeSort (snd (halve xs)))

main = do
    putStrLn $ "Merge: " ++ show (merge [1,3,5] [2,4,6])
    putStrLn $ "Merge Sort [3,5,1,0,3,7,10,2]: " ++ show (mergeSort [3,5,1,0,3,7,10,2])
