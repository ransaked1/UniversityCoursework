--Define the higher-order function curry that takes a function that converts functions on pairs in to
--their curried form that accept their arguments one by one.
curry1 :: ((a,b) -> c) -> a -> b -> c
curry1 f = \x y -> f (x, y)

--Define the function uncurry that provides the inverse to this.
uncurry1 :: (a -> b -> c) -> ((a, b) -> c)
uncurry1 f = \(x,y) -> f x y

main = do
    putStrLn $ "curry fst 1 2: " ++ show (curry1 fst 1 2)
    putStrLn $ "uncurry1 (+) (1, 2): " ++ show (uncurry1 (+) (1, 2))