-- src/Core/Pretty.hs
module Core.Pretty (ppC, ppV, ppBlock) where

import Core.AST

-- -----------------------------------------------------------
--  Estilo compacto (una línea) -- lo usamos a veces
-- -----------------------------------------------------------
ppC :: CExpr -> String
ppC (CNum n)    = show n
ppC (CBool b)   = if b then "true" else "false"
ppC (CVar x)    = x
ppC (CLam x e)  = "(λ" ++ x ++ ". " ++ ppC e ++ ")"
ppC (CApp f a)  = "(" ++ ppC f ++ " " ++ ppC a ++ ")"
ppC (CIf c t e) = "(if "  ++ ppC c ++ " " ++ ppC t ++ " " ++ ppC e ++ ")"
ppC (CIf0 e t f)= "(if0 " ++ ppC e ++ " " ++ ppC t ++ " " ++ ppC f ++ ")"
ppC (CLet x a b)= "(let " ++ x ++ " = " ++ ppC a ++ " in " ++ ppC b ++ ")"
ppC (CAdd a b)  = "(" ++ ppC a ++ " + " ++ ppC b ++ ")"
ppC (CSub a b)  = "(" ++ ppC a ++ " - " ++ ppC b ++ ")"
ppC (CMul a b)  = "(" ++ ppC a ++ " * " ++ ppC b ++ ")"
ppC (CDiv a b)  = "(" ++ ppC a ++ " / " ++ ppC b ++ ")"
ppC (CAdd1 e)   = "(add1 " ++ ppC e ++ ")"
ppC (CSub1 e)   = "(sub1 " ++ ppC e ++ ")"
ppC (CSqrt e)   = "(sqrt " ++ ppC e ++ ")"
ppC (CExpt a b) = "(expt " ++ ppC a ++ " " ++ ppC b ++ ")"
ppC (CEq a b)   = "(" ++ ppC a ++ " = "  ++ ppC b ++ ")"
ppC (CNe a b)   = "(" ++ ppC a ++ " != " ++ ppC b ++ ")"
ppC (CLt a b)   = "(" ++ ppC a ++ " < "  ++ ppC b ++ ")"
ppC (CLe a b)   = "(" ++ ppC a ++ " <= " ++ ppC b ++ ")"
ppC (CGt a b)   = "(" ++ ppC a ++ " > "  ++ ppC b ++ ")"
ppC (CGe a b)   = "(" ++ ppC a ++ " >= " ++ ppC b ++ ")"
ppC (CPair a b) = "(" ++ ppC a ++ " , " ++ ppC b ++ ")"
ppC CNil        = "[]"
ppC (CCons h t) = "[" ++ go (CCons h t) ++ "]"
  where
    go CNil = ""
    go (CCons x CNil) = ppC x
    go (CCons x xs)   = ppC x ++ ", " ++ go xs
    go other = "| " ++ ppC other -- por si llegara algo no canónico

-- -----------------------------------------------------------
--  Estilo BLOQUES con sangría (como en las notas del profe)
-- -----------------------------------------------------------
ppBlock :: CExpr -> String
ppBlock = go 0
  where
    ind n = replicate (2*n) ' '
    line n s = ind n ++ s

    -- let en formato (let (x a) body) con sangría
    go n (CLet x a b) =
      line n ("(let (" ++ x ++ " " ++ go1 a ++ ")") ++ "\n"
      ++ go (n+1) b ++ ")"

    -- if / if0 compactos (una línea)
    go n (CIf c t e)  = line n ("(if "  ++ go1 c ++ " " ++ go1 t ++ " " ++ go1 e ++ ")")
    go n (CIf0 e t f) = line n ("(if0 " ++ go1 e ++ " " ++ go1 t ++ " " ++ go1 f ++ ")")

    -- aplicación y lambda
    go n (CLam x e)   = line n ("(lambda (" ++ x ++ ") " ++ go1 e ++ ")")
    go n (CApp f a)   = line n ("(" ++ go1 f ++ " " ++ go1 a ++ ")")

    -- operadores prefijo en una línea
    go n (CAdd a b) = line n ("(+ "  ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CSub a b) = line n ("(- "  ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CMul a b) = line n ("(* "  ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CDiv a b) = line n ("(/ "  ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CEq  a b) = line n ("(= "  ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CNe  a b) = line n ("(!= " ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CLt  a b) = line n ("(< "  ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CLe  a b) = line n ("(<= " ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CGt  a b) = line n ("(> "  ++ go1 a ++ " " ++ go1 b ++ ")")
    go n (CGe  a b) = line n ("(>= " ++ go1 a ++ " " ++ go1 b ++ ")")

    -- pares y listas
    go n (CPair a b) = line n ("(" ++ go1 a ++ " , " ++ go1 b ++ ")")
    go _ CNil        = "[]"
    go n (CCons h t) = line n ("[" ++ listElems (CCons h t) ++ "]")
      where
        listElems CNil = ""
        listElems (CCons x CNil) = go1 x
        listElems (CCons x xs)   = go1 x ++ ", " ++ listElems xs
        listElems other = "| " ++ go1 other

    -- literales / variables
    go n (CNum k)   = line n (show k)
    go n (CBool b)  = line n (if b then "true" else "false")
    go n (CVar x)   = line n x

    -- Versión inline para anidar en una línea
    go1 (CLet x a b) = "(let (" ++ x ++ " " ++ go1 a ++ ") " ++ go1 b ++ ")"
    go1 (CIf c t e)  = "(if "  ++ go1 c ++ " " ++ go1 t ++ " " ++ go1 e ++ ")"
    go1 (CIf0 e t f) = "(if0 " ++ go1 e ++ " " ++ go1 t ++ " " ++ go1 f ++ ")"
    go1 (CLam x e)   = "(lambda (" ++ x ++ ") " ++ go1 e ++ ")"
    go1 (CApp f a)   = "(" ++ go1 f ++ " " ++ go1 a ++ ")"
    go1 (CAdd a b)   = "(+ "  ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CSub a b)   = "(- "  ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CMul a b)   = "(* "  ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CDiv a b)   = "(/ "  ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CEq  a b)   = "(= "  ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CNe  a b)   = "(!= " ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CLt  a b)   = "(< "  ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CLe  a b)   = "(<= " ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CGt  a b)   = "(> "  ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CGe  a b)   = "(>= " ++ go1 a ++ " " ++ go1 b ++ ")"
    go1 (CPair a b)  = "(" ++ go1 a ++ " , " ++ go1 b ++ ")"
    go1 CNil         = "[]"
    go1 (CCons h t)  = "[" ++ listElems (CCons h t) ++ "]"
      where
        listElems CNil = ""
        listElems (CCons x CNil) = go1 x
        listElems (CCons x xs)   = go1 x ++ ", " ++ listElems xs
        listElems other = "| " ++ go1 other
    go1 (CNum k)     = show k
    go1 (CBool b)    = if b then "true" else "false"
    go1 (CVar x)     = x

-- Valores
ppV :: Val -> String
ppV (VNum n)        = show n
ppV (VBool b)       = if b then "true" else "false"
ppV (VClos x _ _)   = "<closure " ++ x ++ ">"
ppV VNil            = "[]"
ppV (VPair a b)     = "(" ++ ppV a ++ " , " ++ ppV b ++ ")"
