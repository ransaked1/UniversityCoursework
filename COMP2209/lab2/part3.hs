-- using if then else
safetail1 :: [a] -> [a]
safetail1 a = if null a then [] else tail a

-- using !!
safetail2 :: [a] -> [a]
safetail2 a | null a = []
    | otherwise = tail a

-- using pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

main = do
    putStrLn $ "tail [1,2,3,4,5,6] : " ++ show (safetail1 [1,2,3,4,5,6])
    putStrLn $ "tail [] : " ++ show (safetail1 ([] :: [Int]))
    putStrLn $ "tail [1,2,3,4,5,6] : " ++ show (safetail2 [1,2,3,4,5,6])
    putStrLn $ "tail [] : " ++ show (safetail2 ([] :: [Int]))
    putStrLn $ "tail [1,2,3,4,5,6] : " ++ show (safetail3 [1,2,3,4,5,6])
    putStrLn $ "tail [] : " ++ show (safetail3 ([] :: [Int]))