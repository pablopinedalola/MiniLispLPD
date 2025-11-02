module Core.Eval.Natural (eval) where
import Core.AST
lookupEnv :: Var -> Env -> Maybe Val
lookupEnv _ [] = Nothing
lookupEnv x ((y,v):ys) | x==y = Just v | otherwise = lookupEnv x ys
extend :: Var -> Val -> Env -> Env
extend x v rho = (x,v):rho
eval :: Env -> CExpr -> Either String Val
eval _   (CNum n)   = Right (VNum n)
eval _   (CBool b)  = Right (VBool b)
eval rho (CVar x)   = maybe (Left $ "Variable no ligada: "++x) Right (lookupEnv x rho)
eval rho (CLam x e) = Right (VClos x e rho)
eval rho (CApp f a) = do
  vf <- eval rho f; va <- eval rho a
  case vf of VClos x body rho' -> eval (extend x va rho') body
             _ -> Left "Aplicación a no-función"
