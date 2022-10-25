--Implement Euclid's algorithm for calculating the greatest common divisor of two non-negative integers:
--if the two numbers are equal, this number is the result, otherwise, subtract the small from the larger and repeat.
euclid :: Int -> Int -> Int
euclid x y = if x < y then euclid x (y-x)
    else if x > y then euclid (x-y) y
    else x

main = do
    putStrLn $ "Euclid 7 14: " ++ show (euclid 7 14)
    putStrLn $ "Euclid 14 7: " ++ show (euclid 14 7)
    putStrLn $ "Euclid 7 7: " ++ show (euclid 7 7)