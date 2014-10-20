module TypeInference(runTypeInference) where
import Syntax
import TypeInferenceMonad

typeInference :: Expr -> TypeInference Type
typeInference (Var id) = lookupEnv id
typeInference (Num  _) = return TInt
typeInference (Lambda id expr) = do
    i <- fresh
    t <- extendEnv (id, i) (typeInference expr)
    return $ i :->: t
typeInference (expr0 :@: expr1) = do
    i <- fresh
    t0 <- typeInference expr0
    t1 <- typeInference expr1
    _ <- addConstraints [(t0,t1 :->: i)]
    return i

runTypeInference :: Expr -> (Type, Constraints)
runTypeInference expr = run $ typeInference expr
