takeFst :: [a] -> [a]
takeFst [] = []
takeFst (x:xs) = [x]

takeRest :: [a] -> [a]
takeRest [] = []
takeRest (x:xs) = xs

checkAllEmpty :: (Integral a, Eq a) => [[a]] -> Bool
checkAllEmpty lsts = not (any (True==) (map (\lst -> lst /= []) lsts))

multiZipL :: (Integral a, Show a) => [[a]] -> [[a]]
multiZipL lsts | checkAllEmpty lsts = []
               | otherwise = (concat $ map (\lst -> takeFst lst) lsts) : multiZipL (map (\lst -> takeRest lst) lsts)

main = do
    putStrLn $ "multiZipL [[1,2,3],[4,5,6],[7,8,9]]: " ++ show (multiZipL [[1,2,3],[4,5,6],[7,8,9]])
    putStrLn $ "multiZipL [[1,2,3],[4,5,6],[7,8,9],[10],[11],[12,13,14,15]]: " ++ show (multiZipL [[1,2,3],[4,5,6],[7,8,9],[10],[11],[12,13,14,15]])
    putStrLn $ "multiZipL [[],[],[]]: " ++ show (multiZipL [[],[],[]])