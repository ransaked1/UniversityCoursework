data Expr = Var String | Lam String Expr | App Expr Expr | Cl String Expr Environment
  deriving (Eq, Show, Read)

type Environment = [ (String,Expr) ]

lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key [] =  Nothing
lookup key ((x,y):ps)  | key == x =  Just y
                       | otherwise = lookup key ps