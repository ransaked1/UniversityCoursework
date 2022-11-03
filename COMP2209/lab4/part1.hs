--Decide if all elements of a list satisfy a predicate
all1 :: Eq a => (a -> Bool) -> [a] -> Bool
all1 f xs = if (filter f xs) == xs then True else False

--Decide if any element of a list satisfies a predicate
any1 :: (a -> Bool) -> [a] -> Bool
any1 f xs = if length (filter f xs) > 0 then True else False

--Select the initial elements from a list while they satisfy a predicate
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f = foldr (\x acc -> if f x then x : acc else []) []

--Remove the initial elements from a list while they satisfy a predicate
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 f xs = drop (length (foldr (\x acc -> if f x then x : acc else []) [] xs )) xs

main = do
    putStrLn $ "all True: " ++ show (all1 even [4,6,8])
    putStrLn $ "all False: " ++ show (all1 even [3,6,8])
    putStrLn $ "all False: " ++ show (all1 even [8,6,7])
    putStrLn $ "--------------------------------"
    putStrLn $ "any True: " ++ show (any1 even [5,6,8])
    putStrLn $ "any False: " ++ show (any1 even [3,7,9])
    putStrLn $ "--------------------------------"
    putStrLn $ "takeWhile1 (<3) [1,2,3,4,5]: " ++ show (takeWhile1 (<3) [1,2,3,4,5])
    putStrLn $ "takeWhile1 (>3) [1,2,3,4,5]: " ++ show (takeWhile1 (>3) [1,2,3,4,5])
    putStrLn $ "takeWhile1 odd [1,3,5,7,9,10,11,13,15,17]: " ++ show (takeWhile1 odd [1,3,5,7,9,10,11,13,15,17])
    putStrLn $ "takeWhile1 ('w'>) 'hello world': " ++ show (takeWhile1 ('w'>) "hello world")
    putStrLn $ "--------------------------------"
    putStrLn $ "dropWhile1 (<3) [1,2,3,4,5]: " ++ show (dropWhile1 (<3) [1,2,3,4,5])
    putStrLn $ "dropWhile1 (>3) [1,2,3,4,5]: " ++ show (dropWhile1 (>3) [1,2,3,4,5])
    putStrLn $ "dropWhile1 even [2,4,6,7,9,11,12,13,14]: " ++ show (dropWhile1 even [2,4,6,7,9,11,12,13,14])
    putStrLn $ "dropWhile1 ('w'>) 'hello world': " ++ show (dropWhile1 ('w'>) "hello world")