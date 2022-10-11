second xs = head (tail xs)
second :: [Char] -> [Char]

swap (x,y) = (y,x)
swap :: (a,b) -> (b,a)

pair x y = (x,y)
pair :: a -> a -> (a,a)

double x = x*2
double :: Num -> Num

palindrome xs = reverse xs == xs
palindrome :: [Char] -> Bool

twice f x = f ( f x )
twice :: (a -> b) -> a -> a
