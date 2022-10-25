-- using reverse
lastone1 :: [a] -> a
lastone1 a = head (reverse a)

-- using recursion
lastone2 :: [a] -> a
lastone2 [x] = x
lastone2 (_:xs) = lastone2 xs

main = do
    putStrLn $ "last [5,2,8,8,9,10] : " ++ show (lastone1 [5,2,8,8,9,10])
    putStrLn $ "last [5,2,8,8,9,10] : " ++ show (lastone2 [5,2,8,8,9,10])