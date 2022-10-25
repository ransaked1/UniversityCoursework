--Using a list comprehension, give an expression that calculates the sum of the squares of
--odd numbers and cubes of even numbers for the first 100 integers.
comprehension :: Int
comprehension = sum [ if even x then x*x*x else x*x | x <- [1..100]]

main = do
    putStrLn $ "The sum: " ++ show (comprehension)