quicktros :: [Int] -> [Int]
quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
                   where
                     ls = [ a | a <- xs , a >= x ]
                     rs = [ a | a <- xs , a < x ]
                     
main = do
    putStrLn $ "Sorted [5,2,8,8,9,10]: " ++ show (quicktros [5,2,8,8,9,10])