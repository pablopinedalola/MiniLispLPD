module Desugar where
import qualified Surface.AST as S
import qualified Core.AST    as C

-- Traducción principal
desugar :: S.SExpr -> C.CExpr
desugar (S.SNum n)        = C.CNum n
desugar (S.SBool b)       = C.CBool b
desugar (S.SVar x)        = C.CVar x
desugar (S.SLam xs e)     = foldr C.CLam (desugar e) (if null xs then ["_u"] else xs)
desugar (S.SApp f as)     = desugarApp f as
desugar (S.SIf  c t e)    = C.CIf  (desugar c) (desugar t) (desugar e)
desugar (S.SIf0 e t f)    = C.CIf0 (desugar e) (desugar t) (desugar f)
desugar (S.SLet bs body)      = foldr (\(x,e) acc -> C.CLet x (desugar e) acc) (desugar body) bs
desugar (S.SLetStar bs body)  = foldr (\(x,e) acc -> C.CLet x (desugar e) acc) (desugar body) bs
desugar (S.SCond arms eElse)  = desugar (nestIf arms eElse)
desugar (S.SPair a b)     = C.CPair (desugar a) (desugar b)
desugar (S.SList xs)      = listToCons (map desugar xs)

-- (cond [c e] ... [else e])  ==>  (if c e (if ... eElse))
nestIf :: [(S.SExpr, S.SExpr)] -> S.SExpr -> S.SExpr
nestIf [] eElse = eElse
nestIf ((c,e):xs) eElse = S.SIf c e (nestIf xs eElse)

-- Aplicaciones especiales: operadores/predicados variádicos
desugarApp :: S.SExpr -> [S.SExpr] -> C.CExpr
-- Operadores aritméticos
desugarApp (S.SVar "+") as = foldBin C.CAdd as
desugarApp (S.SVar "*") as = foldBin C.CMul as
desugarApp (S.SVar "-") []     = error "(-) requiere al menos 1 argumento"
desugarApp (S.SVar "-") [e]    = C.CSub (C.CNum 0) (desugar e)
desugarApp (S.SVar "-") (e:es) = foldl (\acc a -> C.CSub acc (desugar a)) (desugar e) es
desugarApp (S.SVar "/") []     = error "(/) requiere al menos 1 argumento"
desugarApp (S.SVar "/") (e:es) = foldl (\acc a -> C.CDiv acc (desugar a)) (desugar e) es
desugarApp (S.SVar "add1") [e] = C.CAdd1 (desugar e)
desugarApp (S.SVar "sub1") [e] = C.CSub1 (desugar e)
desugarApp (S.SVar "sqrt") [e] = C.CSqrt (desugar e)
desugarApp (S.SVar "expt") [a,b] = C.CExpt (desugar a) (desugar b)

-- Predicados variádicos: cadenas y todos-distintos
desugarApp (S.SVar "=")  as = chainAnd (chain2 C.CEq  as)
desugarApp (S.SVar "<")  as = chainAnd (chain2 C.CLt  as)
desugarApp (S.SVar "<=") as = chainAnd (chain2 C.CLe  as)
desugarApp (S.SVar ">")  as = chainAnd (chain2 C.CGt  as)
desugarApp (S.SVar ">=") as = chainAnd (chain2 C.CGe  as)
-- (!= e1 ... en)  ⇒  and [e_i != e_j | i<j]
desugarApp (S.SVar "!=") as = chainAnd [ C.CNe (desugar ai) (desugar aj) | (i,ai) <- ixs, (j,aj) <- ixs, i<j ]
  where ixs = zip [0..] as

-- Caso general: aplicación normal
desugarApp f as = foldl C.CApp (desugar f) (map desugar as)

-- Helpers para plegar binarios (n>=2)
foldBin :: (C.CExpr -> C.CExpr -> C.CExpr) -> [S.SExpr] -> C.CExpr
foldBin _ []       = error "operador variádico requiere al menos 2 argumentos"
foldBin _ [_]      = error "operador variádico requiere al menos 2 argumentos"
foldBin op (a:b:rest) = foldl (\acc x -> op acc (desugar x)) (op (desugar a) (desugar b)) rest

-- and booleana con if anidados: and [p1,p2,...] ≡ if p1 (if p2 (... true) false) false
chainAnd :: [C.CExpr] -> C.CExpr
chainAnd []     = C.CBool True
chainAnd (p:ps) = C.CIf p (chainAnd ps) (C.CBool False)

-- para cadenas binarias adyacentes: [e1,e2,e3] con op ⇒ [op e1 e2, op e2 e3]
chain2 :: (C.CExpr -> C.CExpr -> C.CExpr) -> [S.SExpr] -> [C.CExpr]
chain2 _ []  = []
chain2 _ [_] = []
chain2 op (x:y:rest) = op (desugar x) (desugar y) : chain2 op (y:rest)

-- Listas
listToCons :: [C.CExpr] -> C.CExpr
listToCons []     = C.CNil
listToCons (x:xs) = C.CCons x (listToCons xs)
