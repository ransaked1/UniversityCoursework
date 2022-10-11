main = do
    putStrLn $ "2^3*4: " ++ show (2^3*4)
    putStrLn $ "(2^3)*4: " ++ show ((2^3)*4)

    putStrLn $ "2*3+4*5: " ++ show (2*3+4*5)
    putStrLn $ "(2*3)+(4*5): " ++ show ((2*3)+(4*5))

    putStrLn $ "2+3*4^5: " ++ show (2+3*4^5)
    putStrLn $ "2+(3*(4^5)): " ++ show (2+(3*(4^5)))

    putStrLn $ "2^2+2^2: " ++ show (2^2+2^2)
    putStrLn $ "(2^2)+(2^2): " ++ show ((2^2)+(2^2))