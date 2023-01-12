data Frame = HoleApp Expr Environment | AppHole Expr
type Kontinuation = [ Frame ]
type Configuration = (Expr,Environment,Kontinuation)