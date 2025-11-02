{
module Surface.Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Za-z_]
$alnum = [A-Za-z0-9_]

tokens :-

  -- Blancos ASCII (cada uno consume y no emite token)
  " "  ;
  "\t" ;
  "\n" ;
  "\r" ;
  "\f" ;
  "\v" ;

  -- Blancos “raros” comunes en copy-paste (NBSP y amigos)
  "\xA0" ;        -- NO-BREAK SPACE
  "\x85"  ;       -- NEXT LINE
  "\x2000" ; "\x2001" ; "\x2002" ; "\x2003" ; "\x2004" ; "\x2005" ;
  "\x2006" ; "\x2007" ; "\x2008" ; "\x2009" ; "\x200A" ;
  "\x200B" ;      -- ZERO WIDTH SPACE
  "\x2028" ; "\x2029" ;
  "\x3000" ;

  -- Comentarios de línea
  ";" [^\n]* ;

  -- Booleanos
  "true"  { \_ -> TBool True }
  "false" { \_ -> TBool False }

  -- Palabras clave
  "if"     { \_ -> TIf }
  "if0"    { \_ -> TIf0 }
  "let*"   { \_ -> TLetStar }
  "let"    { \_ -> TLet }
  "lambda" { \_ -> TLambda }
  "cond"   { \_ -> TCond }
  "else"   { \_ -> TElse }

  -- Delimitadores
  "("  { \_ -> TLParen }
  ")"  { \_ -> TRParen }
  "["  { \_ -> TLBrack }
  "]"  { \_ -> TRBrack }
  ","  { \_ -> TComma }

  -- Números (con signo opcional)
  "-"? $digit+ { \s -> TInt (read s) }

  -- Operadores / predicados
  "!=" { \_ -> TIdent "!=" }
  "<=" { \_ -> TIdent "<=" }
  ">=" { \_ -> TIdent ">=" }
  "="  { \_ -> TIdent "=" }
  "<"  { \_ -> TIdent "<" }
  ">"  { \_ -> TIdent ">" }
  "+"  { \_ -> TIdent "+" }
  "-"  { \_ -> TIdent "-" }
  "*"  { \_ -> TIdent "*" }
  "/"  { \_ -> TIdent "/" }

  -- Identificadores alfabéticos
  $alpha $alnum* { \s -> TIdent s }

  -- Cualquier otro carácter → error claro
  . { \s -> error ("lexical error: caracter no reconocido = " ++ show s) }

{
data Token
  = TIdent String
  | TInt Integer
  | TBool Bool
  | TLParen | TRParen
  | TLBrack | TRBrack
  | TComma
  | TIf | TIf0 | TLet | TLetStar | TLambda | TCond | TElse
  deriving (Eq, Show)
}
