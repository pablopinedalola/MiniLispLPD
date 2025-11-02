module Surface.Parser (parseExpr) where

import Surface.AST
import Surface.Lexer

parseExpr :: String -> Either String SExpr
parseExpr src = do
  let toks = alexScanTokens src
  (e, rest) <- pExpr toks
  case rest of [] -> Right e
               _  -> Left ("Tokens sobrantes: " ++ show rest)

-- ======== Parser básico ========

pExpr :: [Token] -> Either String (SExpr, [Token])
pExpr (TInt n  : ts) = Right (SNum  n, ts)
pExpr (TBool b : ts) = Right (SBool b, ts)
pExpr (TIdent x: ts) = Right (SVar  x, ts)
pExpr (TLParen : ts) = pParen ts
pExpr (TLBrack : ts) = pBracketList ts
pExpr []             = Left "Entrada vacía."
pExpr (t:_)          = Left ("Token inesperado: " ++ show t)

-- ( ... )  → formas especiales o SApp/SPair
pParen :: [Token] -> Either String (SExpr, [Token])
-- (if c t e)
pParen (TIf : ts) = do
  (c, ts1) <- pExpr ts
  (t, ts2) <- pExpr ts1
  (e, ts3) <- pExpr ts2
  case ts3 of
    (TRParen:ts4) -> Right (SIf c t e, ts4)
    _             -> Left "Falta ')' al cerrar (if ...)."

-- (if0 e t e)
pParen (TIf0 : ts) = do
  (e0, ts1) <- pExpr ts
  (t,  ts2) <- pExpr ts1
  (e,  ts3) <- pExpr ts2
  case ts3 of
    (TRParen:ts4) -> Right (SIf0 e0 t e, ts4)
    _             -> Left "Falta ')' al cerrar (if0 ...)."

-- (lambda (x y ...) body)
pParen (TLambda : TLParen : ts) = do
  (params, afterParams) <- pParamList ts
  (body,   rest)        <- pExpr afterParams
  case rest of
    (TRParen:ts2) -> Right (SLam params body, ts2)
    _             -> Left "Falta ')' al cerrar (lambda ...)."

-- (let ((x e) ...) body)
pParen (TLet : TLParen : ts) = do
  (binds, afterBinds) <- pBindings ts
  (body, rest)        <- pExpr afterBinds
  case rest of
    (TRParen:ts2) -> Right (SLet binds body, ts2)
    _             -> Left "Falta ')' al cerrar (let ...)."

-- (let* ((x e) ...) body)
pParen (TLetStar : TLParen : ts) = do
  (binds, afterBinds) <- pBindings ts
  (body, rest)        <- pExpr afterBinds
  case rest of
    (TRParen:ts2) -> Right (SLetStar binds body, ts2)
    _             -> Left "Falta ')' al cerrar (let* ...)."

-- (cond [c e] ... [else e])
pParen (TCond : ts) = do
  (pairs, mElse, rest) <- pCondArms ts
  case rest of
    (TRParen:ts2) ->
      case mElse of
        Nothing     -> Left "cond requiere un caso [else e]."
        Just eElse  -> Right (SCond pairs eElse, ts2)
    _             -> Left "Falta ')' al cerrar (cond ...)."

-- Si no es forma especial, puede ser (e1 , e2) o (f a1 ... an)
pParen ts = do
  (e1, ts1) <- pExpr ts
  case ts1 of
    (TComma:ts2) -> do
      (e2, ts3) <- pExpr ts2
      case ts3 of
        (TRParen:ts4) -> Right (SPair e1 e2, ts4)
        _             -> Left "Falta ')' al cerrar par."
    _ -> do
      (args, after) <- pMany ts1
      case after of
        (TRParen:ts2) ->
          case args of
            []     -> Left "Lista vacía no permitida."
            (a:as) -> Right (SApp e1 (a:as), ts2)
        _ -> Left "Falta ')'."

-- Parámetros: (id id id)
pParamList :: [Token] -> Either String ([String], [Token])
pParamList (TRParen:ts) = Right ([], ts)
pParamList (TIdent x : ts) = do
  (xs, rest) <- pParamList ts
  Right (x:xs, rest)
pParamList _ = Left "Lista de parámetros inválida en (lambda ...)."

-- ( (id e) ... )
pBindings :: [Token] -> Either String ([(String,SExpr)], [Token])
pBindings (TRParen:ts) = Right ([], ts)  -- sin ligaduras
pBindings (TLParen:TIdent x:ts) = do
  (e, rest) <- pExpr ts
  case rest of
    (TRParen:ts1) -> do
      (xs, after) <- pBindings ts1
      Right ((x,e):xs, after)
    _ -> Left "Falta ')' al cerrar una ligadura de let/let*."
pBindings _ = Left "Ligaduras inválidas en (let / let* ...)."

-- (cero o más expr) hasta ')', sin consumir ')'
pMany :: [Token] -> Either String ([SExpr], [Token])
pMany (TRParen:ts) = Right ([], TRParen:ts)
pMany []           = Left "Entrada terminada; faltó ')'."
pMany ts           = do
  (e, ts1)  <- pExpr ts
  (es, ts2) <- pMany ts1
  Right (e:es, ts2)

-- [e1, e2, ...]
pBracketList :: [Token] -> Either String (SExpr, [Token])
pBracketList (TRBrack:ts) = Right (SList [], ts)  -- []
pBracketList ts = do
  (es, after) <- pElems ts
  case after of
    (TRBrack:ts2) -> Right (SList es, ts2)
    _             -> Left "Falta ']' al cerrar lista."

-- e {, e}*
pElems :: [Token] -> Either String ([SExpr], [Token])
pElems ts = do
  (e1, ts1) <- pExpr ts
  pMoreElems [e1] ts1
  where
    pMoreElems acc (TComma:ts') = do
      (e, ts'') <- pExpr ts'
      pMoreElems (acc ++ [e]) ts''
    pMoreElems acc rest         = Right (acc, rest)

-- (cond ...) brazos: [c e] ... [else e]
pCondArms :: [Token] -> Either String ([(SExpr,SExpr)], Maybe SExpr, [Token])
pCondArms (TRParen:ts) = Right ([], Nothing, TRParen:ts)  -- no debería ocurrir (esperábamos brazos)
pCondArms (TLBrack:TElse:ts) = do
  (eElse, rest) <- pExpr ts
  case rest of
    (TRBrack:after) -> Right ([], Just eElse, after)
    _               -> Left "Falta ']' al cerrar [else e]."
pCondArms (TLBrack:ts) = do
  (c, ts1) <- pExpr ts
  (e, ts2) <- pExpr ts1
  case ts2 of
    (TRBrack:after) -> do
      (pairs, mElse, rest) <- pCondArms after
      Right ((c,e):pairs, mElse, rest)
    _ -> Left "Falta ']' al cerrar [c e] en cond."
pCondArms ts = Right ([], Nothing, ts) -- fin de brazos (siguiente debe ser TRParen)
