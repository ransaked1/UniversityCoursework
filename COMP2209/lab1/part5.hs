-- Using < instead of <= will remove duplicates in the final list.
quicksort1 [] = []
quicksort1 (x:xs) = quicksort1 ls ++ [x] ++ quicksort1 rs
                   where
                     ls = [ a | a <- xs , a <= x ]
                     rs = [ a | a <- xs , a > x ]

quicksort2 [] = []
quicksort2 (x:xs) = quicksort2 ls ++ [x] ++ quicksort2 rs
                   where
                     ls = [ a | a <- xs , a < x ]
                     rs = [ a | a <- xs , a > x ]

main = do
    putStrLn $ "Sorted [5,2,8,8,9,10] with <=: " ++ show (quicksort1 [5,2,8,8,9,10])
    putStrLn $ "Sorted [5,2,8,8,9,10] with <: " ++ show (quicksort2 [5,2,8,8,9,10])