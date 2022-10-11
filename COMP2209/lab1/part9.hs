bools :: [Bool]
bools = [True,False,False]

nums :: [[Int]]
nums = [[2,3], [5], [8,3]]

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

copy :: a -> (a,a)
copy x = (x,x)

--Apply a defined function on a value
apply :: (a -> b) -> a -> b
apply x y = x y

--String is already a list of characters.
explode :: String -> [Char]
explode [] = ""
explode (x:xs) = [x] ++ explode xs

main = do
    putStrLn $ "Bools: " ++ show (bools)
    putStrLn $ "Nums: " ++ show (nums)
    putStrLn $ "Add: " ++ show (add 1 2 3)
    putStrLn $ "Copy: " ++ show (copy [3,5])
    putStrLn $ "Apply: " ++ show (apply copy 3)
    putStrLn $ "Explode: " ++ show (explode "Hello World!")
