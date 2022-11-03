--Consider the recursive pattern described by the higher-order function below:
unfold p h t x | p x = []
    | otherwise = h x : unfold p h t (t x)
--Use unfold to define the following functions:

--int2bin - that converts an integer to a binary representation as a list of binary digits.
int2bin :: Int -> [Int]
int2bin nr = reverse (unfold (\x -> x == 0) (\x -> if x `mod` 2 == 1 then 1 else 0) (\x -> x `div` 2) nr)

--chop - that takes a string and chops it in to a list of strings of a given length.
chop :: String -> Int -> [String]
chop s len = unfold (\x -> x == []) (\x -> take len x) (\x -> drop len x) s

--map - the standard map function.
map1 :: (a -> b) -> [a] -> [b]
map1 f xs = unfold (\x -> length x == 0) (\(x:xs) -> f x) (\(x:xs) -> xs) xs

--iterate - that takes a function f and a value x and produces the infinite list [x, f x, f (f x), ...].
iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = unfold (\x -> False) (\x -> f x) (\x -> f x) x

main = do
    putStrLn $ "int2bin 12: " ++ show (int2bin 12)
    putStrLn $ "int2bin 0: " ++ show (int2bin 0)
    putStrLn $ "int2bin 3: " ++ show (int2bin 3)
    putStrLn $ "--------------------------------"
    putStrLn $ "chop 'abcdecfghi' 3: " ++ show (chop "abcdecfghi" 3)
    putStrLn $ "chop 'abcabcabc' 3: " ++ show (chop "abcabcabc" 3)
    putStrLn $ "chop 'ab' 0: " ++ show (chop "ab" 3)
    putStrLn $ "--------------------------------"
    putStrLn $ "map abs [-1,-3,4,-12]: " ++ show (map1 abs [-1,-3,4,-12])
    putStrLn $ "map (3*) [1,2,3,4]: " ++ show (map1 (3*) [1,2,3,4])
    putStrLn $ "map (recip . negate) [1,4,-5,0.1]: " ++ show (map1 (recip . negate) [1,4,-5,0.1])
    putStrLn $ "--------------------------------"
    putStrLn $ "take 10 (iterate (2*) 1): " ++ show (take 10 (iterate1 (2*) 1))
    putStrLn $ "take 10 (iterate (x -> (x+3)*2) 1): " ++ show (take 10 (iterate1 (\x -> (x+3)*2) 1))
