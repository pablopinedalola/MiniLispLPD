-- src/Core/Eval/Natural.hs
-- Semántica natural (big-step) del núcleo.
-- Evalúa en un entorno (lista de pares Var, Val) y expone runEval para usar entorno vacío.

module Core.Eval.Natural (eval, runEval) where

import Core.AST

-- ==== Entorno ===============================================================

lookupEnv :: Var -> Env -> Maybe Val
lookupEnv _ [] = Nothing
lookupEnv x ((y,v):ys)
  | x == y    = Just v
  | otherwise = lookupEnv x ys

extend :: Var -> Val -> Env -> Env
extend x v rho = (x,v):rho

-- ==== Helpers de chequeo de tipos ==========================================

type M = Either String

err :: String -> M a
err = Left

expectNum :: Val -> M Integer
expectNum (VNum n) = Right n
expectNum v        = err $ "Se esperaba número, llegó: " ++ show v

expectBool :: Val -> M Bool
expectBool (VBool b) = Right b
expectBool v         = err $ "Se esperaba booleano, llegó: " ++ show v

-- binaria numérica → número
binNumV :: (Integer -> Integer -> Integer) -> Env -> CExpr -> CExpr -> M Val
binNumV op rho e1 e2 = do
  v1 <- eval rho e1
  v2 <- eval rho e2
  n1 <- expectNum v1
  n2 <- expectNum v2
  pure (VNum (op n1 n2))

-- binaria numérica → booleano
binCmpV :: (Integer -> Integer -> Bool) -> Env -> CExpr -> CExpr -> M Val
binCmpV op rho e1 e2 = do
  v1 <- eval rho e1
  v2 <- eval rho e2
  n1 <- expectNum v1
  n2 <- expectNum v2
  pure (VBool (op n1 n2))

-- unaria numérica
unaNumV :: (Integer -> Integer) -> Env -> CExpr -> M Val
unaNumV op rho e = do
  v <- eval rho e
  n <- expectNum v
  pure (VNum (op n))

-- potencia/sqrt con chequeos simples
exptV :: Env -> CExpr -> CExpr -> M Val
exptV rho a b = do
  v1 <- eval rho a >>= expectNum
  v2 <- eval rho b >>= expectNum
  pure (VNum (v1 ^ v2))

sqrtV :: Env -> CExpr -> M Val
sqrtV rho e = do
  n <- eval rho e >>= expectNum
  if n < 0 then err "sqrt de negativo"
           else pure (VNum (floor (sqrt (fromIntegral n :: Double))))

-- pares/listas
evalPair :: Env -> CExpr -> CExpr -> M Val
evalPair rho a b = do
  va <- eval rho a
  vb <- eval rho b
  pure (VPair va vb)

evalCons :: Env -> CExpr -> CExpr -> M Val
evalCons rho h t = do
  vh <- eval rho h
  vt <- eval rho t
  pure (VPair vh vt)   -- listas como pares anidados; CNil ↦ VNil

-- aplicar clausura (call-by-value)
apply :: Val -> Val -> M Val
apply (VClos x body rho) arg = eval (extend x arg rho) body
apply v _ = err $ "Aplicación a no-función: " ++ show v

-- ==== Evaluación (big-step) ================================================

eval :: Env -> CExpr -> M Val
-- Literales/variables
eval _   (CNum n)   = Right (VNum n)
eval _   (CBool b)  = Right (VBool b)
eval rho (CVar x)   = maybe (err $ "Variable no ligada: " ++ x) Right (lookupEnv x rho)

-- Lambda y aplicación
eval rho (CLam x e) = Right (VClos x e rho)
eval rho (CApp f a) = do
  vf <- eval rho f
  va <- eval rho a
  apply vf va

-- If booleano y If0 numérico
eval rho (CIf c t e) = do
  b <- eval rho c >>= expectBool
  if b then eval rho t else eval rho e

eval rho (CIf0 e t f) = do
  n <- eval rho e >>= expectNum
  if n == 0 then eval rho t else eval rho f

-- Let
eval rho (CLet x e1 e2) = do
  v1 <- eval rho e1
  eval (extend x v1 rho) e2

-- Aritmética
eval rho (CAdd e1 e2) = binNumV (+) rho e1 e2
eval rho (CSub e1 e2) = binNumV (-) rho e1 e2
eval rho (CMul e1 e2) = binNumV (*) rho e1 e2
eval rho (CDiv e1 e2) = do
  n1 <- eval rho e1 >>= expectNum
  n2 <- eval rho e2 >>= expectNum
  if n2 == 0 then err "División entre cero" else pure (VNum (n1 `div` n2))

eval rho (CAdd1 e)   = unaNumV (+1) rho e
eval rho (CSub1 e)   = unaNumV (subtract 1) rho e
eval rho (CSqrt e)   = sqrtV rho e
eval rho (CExpt a b) = exptV rho a b

-- Comparadores
eval rho (CEq e1 e2) = binCmpV (==) rho e1 e2
eval rho (CNe e1 e2) = binCmpV (/=) rho e1 e2
eval rho (CLt e1 e2) = binCmpV (<)  rho e1 e2
eval rho (CLe e1 e2) = binCmpV (<=) rho e1 e2
eval rho (CGt e1 e2) = binCmpV (>)  rho e1 e2
eval rho (CGe e1 e2) = binCmpV (>=) rho e1 e2

-- Pares / Listas
eval rho (CPair a b) = evalPair rho a b
eval rho (CCons h t) = evalCons rho h t
eval _   CNil        = Right VNil

-- Helper: evalúa en entorno vacío
runEval :: CExpr -> M Val
runEval = eval []
