--Using a list comprehension, define a function grid :: Int -> Int -> [(Int,Int)]
--that returns a coordinate grid of a given size.
grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

--Create a function square :: Int -> [(Int,Int)] that produces a square grid but
--excluding the diagonal from (0,0) to (n,n).
square :: Int -> [(Int,Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x/=y]

main = do
    putStrLn $ "Grid: " ++ show (grid 1 2)
    putStrLn $ "Square: " ++ show (square 2)