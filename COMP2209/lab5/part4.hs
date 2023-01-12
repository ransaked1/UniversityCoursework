--Consider the data type
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
p1,p2 :: Prop
p1 = Not (Var 'c')
p2 = And (Not (Var 'c') ) (Const True)
p3 = And (Var 'c') (Const True)
p4 = Imply (Not (Var 'c') ) (Const True)

--A variable occurrence in a Prop value is in negative position if it appears under an odd number of negations.
--A negation is a Not constructor or the first argument of the Imply constructor.

--For example, X is in negative position in Not (Var X)
--but not in Imply (Not (Var X) ) (Const True) as it appears under two negations in the latter.

--We say that a proposition P is in negative form if all occurrences of all of its variables are negative.
--We say that a proposition P is in positive form if all occurrences of all of its variables are positive.
--We say that a proposition P is in mixed form if neither of the above hold.

--Write a function getForm that accepts a proposition and determines which of the above cases holds.

getForm :: Prop -> Bool
getForm (Const a) = a
getForm (Var c) = True
getForm (Not p) = not (getForm p)
getForm (And p1 p2) = (&&) (getForm p1) (getForm p2)
getForm (Imply p1 p2) = if (getForm p1) then (getForm p2) else True

main = do
    putStrLn $ "getForm Not (Var 'c'): " ++ show (getForm p1)
    putStrLn $ "getForm (Not (Var 'c') ) (Const True): " ++ show (getForm p2)
    putStrLn $ "getForm And (Var 'c') (Const True): " ++ show (getForm p3)
    putStrLn $ "getForm Imply (Not (Var 'c') ) (Const True): " ++ show (getForm p4)