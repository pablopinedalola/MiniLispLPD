module Core.Pretty (ppC, ppV) where
import Core.AST

ppC :: CExpr -> String
ppC (CNum n)    = show n
ppC (CBool b)   = if b then "true" else "false"
ppC (CVar x)    = x
ppC (CLam x e)  = "(λ" ++ x ++ ". " ++ ppC e ++ ")"
ppC (CApp f a)  = "(" ++ ppC f ++ " " ++ ppC a ++ ")"
ppC (CIf c t e) = "(if " ++ ppC c ++ " " ++ ppC t ++ " " ++ ppC e ++ ")"
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
    go other = "| " ++ ppC other -- por si llega algo no canónico

ppV :: Val -> String
ppV (VNum n)        = show n
ppV (VBool b)       = if b then "true" else "false"
ppV (VClos x _ _)   = "<closure " ++ x ++ ">"
ppV VNil            = "[]"
ppV (VPair a b)     = "(" ++ ppV a ++ " , " ++ ppV b ++ ")"
