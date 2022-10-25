--Define the standard library function replicate :: Int -> a -> [a] using a list comprehension.
replicateremake :: Int -> a -> [a]
replicateremake rep elem = [ elem | x <- [1..rep]]

main = do
    putStrLn $ "Replicate: " ++ show (replicateremake 3 (1,2))