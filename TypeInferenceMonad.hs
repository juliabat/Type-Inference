module TypeInferenceMonad(TypeInference, fresh, lookupEnv, extendEnv,
    addConstraints, run) where
import Syntax

type Env = [(Ident, Type)]

newtype TypeInference a = TypeInference(Int -> Env -> (a, Int, Constraints))

instance Monad TypeInference where
    (TypeInference aFun) >>= g =
        TypeInference ( \ i env ->
        let (a, i', c) = (aFun i env) in
        let TypeInference bFun = g a in
        let (b, i'', c') = bFun i' env in
        (b, i'', c ++ c'))

    return a =  TypeInference(\ i env -> (a, 0, []))

fresh :: TypeInference Type
fresh = TypeInference (\ i env -> (TVar i, i+1, []))

lookupEnv :: Ident -> TypeInference Type
lookupEnv ident = TypeInference (\ i env ->
    ((lookup' $ lookup ident env), i, []))
    where lookup'  = maybe (error "no binding") id

extendEnv :: (Ident, Type) -> TypeInference a-> TypeInference a
extendEnv binding (TypeInference typeInference) = TypeInference(\ i env ->
    typeInference i (binding : env))

addConstraints :: Constraints -> TypeInference ()
addConstraints constraints = TypeInference $ \ i env -> ((), i, constraints)

run :: TypeInference a -> (a, Constraints)
run (TypeInference aFun) = let (a, _, c) = (aFun 0 []) in (a, c)

