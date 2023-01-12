--Write a fold function for the data type of expressions:
data Expr = Val Int | Add Expr Expr | Sub Expr Expr

--Data type initialization
e :: Expr
e = Sub (Add (Val 5) (Val 6)) (Val 7)

--Write a fold function for the data type of expressions
fold :: (Int -> Int -> Int) -> Int -> Int -> Int
fold f x y = f x y

--Define a function eval that evaluates an expression
eval :: Expr -> Int
eval (Val a) = a
eval (Add a b) = fold (+) (eval a) (eval b)
eval (Sub a b) = fold (-) (eval a) (eval b)

--Define a function size that calculates the size of the AST (in nodes) that represents the expression.
size :: Expr -> Int
size (Val a) = 1
size (Add a b) = 1 + (size a) + (size b)
size (Sub a b) = 1 + (size a) + (size b)

main = do
    putStrLn $ "evalExpr: " ++ show (eval e)
    putStrLn $ "sizeExpr: " ++ show (size e)