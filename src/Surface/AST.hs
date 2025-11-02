module Surface.AST where

data SExpr
  = SNum Integer
  | SBool Bool
  | SVar String
  | SApp SExpr [SExpr]         -- (f a1 ... an)
  | SLam [String] SExpr        -- (lambda (x y ...) body)
  | SIf  SExpr SExpr SExpr     -- (if c t e)
  | SIf0 SExpr SExpr SExpr     -- (if0 e t e)
  | SLet [(String, SExpr)] SExpr      -- (let ((x e) ...) body)
  | SLetStar [(String, SExpr)] SExpr  -- (let* ((x e) ...) body)
  | SCond [(SExpr, SExpr)] SExpr      -- (cond [c e] ... [else e])
  | SPair SExpr SExpr          -- (e1 , e2)
  | SList [SExpr]              -- [e1, e2, ...]
  deriving (Eq, Show)
