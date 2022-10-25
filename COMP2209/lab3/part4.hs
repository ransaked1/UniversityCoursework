--A triple (x,y,z) of positive integers is Pythagorean if it satisfies the equation x^2 + y^2 = z^2.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [0..n], x*x + y*y == z*z]

main = do
    putStrLn $ "Pythagorean: " ++ show (pyths 10)