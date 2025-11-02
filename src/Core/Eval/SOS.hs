module Core.Eval.SOS (step, multiStep, trace) where

import Core.AST
import Core.Pretty (ppC)
import Control.Applicative ((<|>))


-- Sustitución (solo para lambda y let; núcleo pequeño)
subst :: Var -> CExpr -> CExpr -> CExpr
subst x v (CVar y)        | x == y    = v
                          | otherwise = CVar y
subst x v (CLam y e)      | x == y    = CLam y e
                          | otherwise = CLam y (subst x v e)
subst x v (CApp f a)                  = CApp (subst x v f) (subst x v a)
subst x v (CIf c t e)                 = CIf (subst x v c) (subst x v t) (subst x v e)
subst x v (CIf0 e t f)                = CIf0 (subst x v e) (subst x v t) (subst x v f)
subst x v (CLet y e1 e2)  | x == y    = CLet y (subst x v e1) e2
                          | otherwise = CLet y (subst x v e1) (subst x v e2)
subst x v (CAdd a b)                  = CAdd (subst x v a) (subst x v b)
subst x v (CSub a b)                  = CSub (subst x v a) (subst x v b)
subst x v (CMul a b)                  = CMul (subst x v a) (subst x v b)
subst x v (CDiv a b)                  = CDiv (subst x v a) (subst x v b)
subst x v (CAdd1 e)                   = CAdd1 (subst x v e)
subst x v (CSub1 e)                   = CSub1 (subst x v e)
subst x v (CSqrt e)                   = CSqrt (subst x v e)
subst x v (CExpt a b)                 = CExpt (subst x v a) (subst x v b)
subst x v (CEq a b)                   = CEq (subst x v a) (subst x v b)
subst x v (CNe a b)                   = CNe (subst x v a) (subst x v b)
subst x v (CLt a b)                   = CLt (subst x v a) (subst x v b)
subst x v (CLe a b)                   = CLe (subst x v a) (subst x v b)
subst x v (CGt a b)                   = CGt (subst x v a) (subst x v b)
subst x v (CGe a b)                   = CGe (subst x v a) (subst x v b)
subst x v (CPair a b)                 = CPair (subst x v a) (subst x v b)
subst x v (CCons h t)                 = CCons (subst x v h) (subst x v t)
subst _ _ CNil                        = CNil
subst _ _ lit@(CNum _)                = lit
subst _ _ lit@(CBool _)               = lit

isValue :: CExpr -> Bool
isValue (CNum _)   = True
isValue (CBool _)  = True
isValue (CLam _ _) = True
isValue CNil       = True
isValue (CPair a b)= isValue a && isValue b
isValue (CCons _ _) = False -- listas como expresión; valor sería tras desugar canónico
isValue _          = False

-- Operadores primitivos (cuando están listos)
prim2 :: (Integer -> Integer -> Integer) -> CExpr -> CExpr -> Maybe CExpr
prim2 op (CNum n1) (CNum n2) = Just (CNum (op n1 n2))
prim2 _  _          _        = Nothing

prim2b :: (Integer -> Integer -> Bool) -> CExpr -> CExpr -> Maybe CExpr
prim2b op (CNum n1) (CNum n2) = Just (CBool (op n1 n2))
prim2b _  _          _        = Nothing

prim1 :: (Integer -> Integer) -> CExpr -> Maybe CExpr
prim1 op (CNum n) = Just (CNum (op n))
prim1 _  _        = Nothing

-- Un paso de evaluación (call-by-value)
step :: CExpr -> Maybe CExpr
-- let
step (CLet x e1 e2)
  | isValue e1 = Just (subst x e1 e2)
  | otherwise  = CLet x <$> step e1 <*> pure e2

-- beta-v
step (CApp (CLam x e) v)
  | isValue v  = Just (subst x v e)
step (CApp f a)
  | isValue f  = CApp f <$> step a
  | otherwise  = (`CApp` a) <$> step f

-- if/if0
step (CIf (CBool True)  t _) = Just t
step (CIf (CBool False) _ f) = Just f
step (CIf c t e)             = (\c' -> CIf c' t e) <$> step c

step (CIf0 (CNum 0)  t _) = Just t
step (CIf0 (CNum _)  _ f) = Just f
step (CIf0 e t f)         = (\e' -> CIf0 e' t f) <$> step e

-- aritmética/comparadores
step (CAdd a b)
  | isValue a && isValue b = prim2 (+) a b
  | isValue a              = (\b' -> CAdd a b') <$> step b
  | otherwise              = (\a' -> CAdd a' b) <$> step a
step (CSub a b)
  | isValue a && isValue b = prim2 (-) a b
  | isValue a              = (\b' -> CSub a b') <$> step b
  | otherwise              = (\a' -> CSub a' b) <$> step a
step (CMul a b)
  | isValue a && isValue b = prim2 (*) a b
  | isValue a              = (\b' -> CMul a b') <$> step b
  | otherwise              = (\a' -> CMul a' b) <$> step a
step (CDiv a b)
  | isValue a && isValue b =
      case (a,b) of (CNum n1, CNum 0) -> Nothing
                    (CNum n1, CNum n2) -> Just (CNum (n1 `div` n2))
                    _ -> Nothing
  | isValue a              = (\b' -> CDiv a b') <$> step b
  | otherwise              = (\a' -> CDiv a' b) <$> step a

step (CAdd1 e) = CAdd1 <$> step e <|> prim1 (+1) e
step (CSub1 e) = CSub1 <$> step e <|> prim1 (subtract 1) e

step (CEq a b)
  | isValue a && isValue b = prim2b (==) a b
  | isValue a              = (\b' -> CEq a b') <$> step b
  | otherwise              = (\a' -> CEq a' b) <$> step a
step (CNe a b)
  | isValue a && isValue b = prim2b (/=) a b
  | isValue a              = (\b' -> CNe a b') <$> step b
  | otherwise              = (\a' -> CNe a' b) <$> step a
step (CLt a b)
  | isValue a && isValue b = prim2b (<) a b
  | isValue a              = (\b' -> CLt a b') <$> step b
  | otherwise              = (\a' -> CLt a' b) <$> step a
step (CLe a b)
  | isValue a && isValue b = prim2b (<=) a b
  | isValue a              = (\b' -> CLe a b') <$> step b
  | otherwise              = (\a' -> CLe a' b) <$> step a
step (CGt a b)
  | isValue a && isValue b = prim2b (>) a b
  | isValue a              = (\b' -> CGt a b') <$> step b
  | otherwise              = (\a' -> CGt a' b) <$> step a
step (CGe a b)
  | isValue a && isValue b = prim2b (>=) a b
  | isValue a              = (\b' -> CGe a b') <$> step b
  | otherwise              = (\a' -> CGe a' b) <$> step a

-- pares
step (CPair a b)
  | isValue a && not (isValue b) = (\b' -> CPair a b') <$> step b
  | not (isValue a)              = (\a' -> CPair a' b) <$> step a
  | otherwise                    = Nothing

-- listas (si llegan como núcleo con CCons)
step (CCons h t)
  | isValue h && not (isValue t) = (\t' -> CCons h t') <$> step t
  | not (isValue h)              = (\h' -> CCons h' t) <$> step h
  | otherwise                    = Nothing
step CNil = Nothing

-- valores
step v | isValue v = Nothing

-- por defecto
step _ = Nothing

-- multistep y traza
multiStep :: Int -> CExpr -> [CExpr]
multiStep 0 e = [e]
multiStep k e = case step e of
  Nothing -> [e]
  Just e' -> e : multiStep (k-1) e'

trace :: CExpr -> String
trace e = unlines (map ppC (multiStep 100 e))
  where
    -- límite 100 pasos: evita loops en demos
    _ = ()
