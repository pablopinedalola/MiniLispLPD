-- app/Main.hs
module Main where

import qualified Surface.Parser as P
import qualified Desugar        as D
import System.Environment (getArgs)
import System.IO (hIsTerminalDevice, stdin)
import Data.Char (isSpace)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Modo no interactivo: lee TODO stdin (una expresión) y procesa
runOne :: (String -> String) -> IO ()
runOne render = do
  src <- fmap trim getContents
  if null src
     then putStrLn "Error: entrada vacía en stdin."
     else case P.parseExpr src of
            Left e   -> putStrLn ("Error: " ++ e)
            Right se -> putStrLn (render (showOut se))
  where
    -- showOut decide qué imprimir a partir del AST de superficie
    showOut se = case render "" of
      -- truco: usamos el 'render' para saber si nos pidieron 'surf' o 'core'
      -- Implementamos dos lambdas distintas abajo.
      _ -> case renderTag of
             "surf" -> show se
             "core" -> show (D.desugar se)
             _      -> show se
    renderTag = renderTagExtract render
    -- identificador del render solicitado
    renderTagExtract f | f "" == "surf" = "surf"
                       | f "" == "core" = "core"
                       | otherwise      = "surf"

-- “Constructores” de render
renderSurf :: String -> String
renderSurf _ = "surf"

renderCore :: String -> String
renderCore _ = "core"

-- REPL interactivo de siempre
loop :: IO ()
loop = do
  putStrLn "Opciones: s) AST superficie | c) AST núcleo (desazucar) | q) salir"
  putStr "> "
  cmd <- getLine
  case cmd of
    "q" -> putStrLn "Adiós."
    "s" -> do
      putStrLn "Expr:"
      src <- getLine
      case P.parseExpr src of
        Left e   -> putStrLn ("Error: " ++ e)
        Right se -> print se
      loop
    "c" -> do
      putStrLn "Expr:"
      src <- getLine
      case P.parseExpr src of
        Left e   -> putStrLn ("Error: " ++ e)
        Right se -> print (D.desugar se)
      loop
    _   -> loop

main :: IO ()
main = do
  args <- getArgs
  tty  <- hIsTerminalDevice stdin
  case (args, tty) of
    (["surf"], _) -> runOne renderSurf
    (["core"], _) -> runOne renderCore
    _             -> loop
