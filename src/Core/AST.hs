module Core.AST where
type Var = String

data CExpr
  = CNum Integer
  | CBool Bool
  | CVar Var
  | CLam Var CExpr
  | CApp CExpr CExpr
  | CIf  CExpr CExpr CExpr     -- if booleano
  | CIf0 CExpr CExpr CExpr     -- if0 numérico
  | CLet Var CExpr CExpr       -- let x = e1 in e2
  -- Aritmética y funciones
  | CAdd CExpr CExpr | CSub CExpr CExpr | CMul CExpr CExpr | CDiv CExpr CExpr
  | CAdd1 CExpr | CSub1 CExpr | CSqrt CExpr | CExpt CExpr CExpr
  -- Comparadores
  | CEq CExpr CExpr | CNe CExpr CExpr
  | CLt CExpr CExpr | CLe CExpr CExpr | CGt CExpr CExpr | CGe CExpr CExpr
  -- Pares y listas
  | CPair CExpr CExpr
  | CCons CExpr CExpr
  | CNil
  deriving (Eq, Show)

-- Valores (para más adelante)
data Val
  = VNum Integer
  | VBool Bool
  | VClos Var CExpr Env
  | VPair Val Val
  | VNil
  deriving (Eq, Show)

type Env = [(Var, Val)]
