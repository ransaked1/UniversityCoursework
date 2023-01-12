--Write code to make the type constructor (Pair a) into a Functor.
data Pair a b = Pair (a,b)
instance Functor (Pair a) where
    fmap f (Pair (a,b)) = Pair ((f a),(f b))
    --fmap :: (a -> b -> (a, b)) -> Pair a b -> Pair a b

--Write code to make the type constructor (Fun a) into a Functor.
data F a b = F (a -> b)
instance Functor (F a) where
    fmap f (F a) = F (\n -> (f n))

main = do
    putStrLn $ "evalExpr: "