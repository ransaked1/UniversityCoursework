evalLoop :: Expr -> Expr
evalLoop e = fst $ unpack $ evalLoop' (e,[],[])
    where evalLoop' (e,env,k) = case eval1 (e,env,k) of
        (w@(Cl _ _ _), _ , []) -> w
        c -> evalLoop' c