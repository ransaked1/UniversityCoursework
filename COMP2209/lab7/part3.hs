-- Function to update an environment with a new binding
update :: Environment -> String -> Expr -> Environment
update env x e = (x,e) : env

-- Function to unpack a closure to extract the underlying lambda term and environment
unpack :: Expr -> (Expr,Environment)
unpack (Cl x e env1) = ((Lam x e) , env1)
unpack _ = error "Cannont unpack closure"

checkLookup :: Maybe Expr -> Expr
checkLookup (Nothing) = error "Unbound variable found"
checkLookup (Just e) = e

isValue :: Expr -> Bool
isValue (Cl _ _ _) = True
isValue _ = False

--Small step evaluation function
eval1 :: Configuration -> Configuration
eval1 ((Var x),env,k) = (e',env',k)
      where (e',env') = (unpack $ checkLookup $ lookup x env)
eval1 ((App e1 e2),env,k) = (e1,env, (HoleApp e2 env) : k)
eval1 ((Lam x e),env,k) = ((Cl x e env), env, k)
eval1 (w,env1,(HoleApp e env2):k ) | isValue w = (e, env2, (AppHole w) : k)
eval1 (w,env1,(AppHole (Cl x e env2) ) : k ) | isValue w  = (e, update env2 x w, k)
eval1 c@((Cl _ _ _),_,[]) = c
eval1 (_) = error "Evaluation Error"