-- app/Main.hs
module Main where

import qualified Surface.Parser    as P
import qualified Desugar           as D
import qualified Core.Eval.Natural as N
import qualified Core.Pretty       as PP
import qualified Core.Eval.SOS     as SOS
import System.Environment (getArgs)
import System.IO (hIsTerminalDevice, stdin)
import Data.Char (isSpace)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Modo no-interactivo: "surf" | "core" | "eval" | "trace"
runOne :: String -> IO ()
runOne mode = do
  src <- fmap trim getContents
  if null src
    then putStrLn "Error: entrada vacía en stdin."
    else case P.parseExpr src of
      Left e   -> putStrLn ("Error: " ++ e)
      Right se -> case mode of
        "surf"  -> print se
        "core"  -> print (D.desugar se)
        "eval"  -> case N.runEval (D.desugar se) of
                     Left err -> putStrLn ("Error: " ++ err)
                     Right v  -> putStrLn (PP.ppV v)
        "trace" -> putStrLn (SOS.trace (D.desugar se))
        _       -> print se

loop :: IO ()
loop = do
  putStrLn "Opciones: s) AST superficie | c) AST núcleo | e) evaluar | t) traza SOS | q) salir"
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
    "e" -> do
      putStrLn "Expr:"
      src <- getLine
      case P.parseExpr src of
        Left e   -> putStrLn ("Error: " ++ e)
        Right se -> case N.runEval (D.desugar se) of
                      Left err -> putStrLn ("Error: " ++ err)
                      Right v  -> putStrLn (PP.ppV v)
      loop
    "t" -> do
      putStrLn "Expr:"
      src <- getLine
      case P.parseExpr src of
        Left e   -> putStrLn ("Error: " ++ e)
        Right se -> putStrLn (SOS.trace (D.desugar se))
      loop
    _   -> loop

main :: IO ()
main = do
  args <- getArgs
  tty  <- hIsTerminalDevice stdin
  case (args, tty) of
    (["surf"],  _) -> runOne "surf"
    (["core"],  _) -> runOne "core"
    (["eval"],  _) -> runOne "eval"
    (["trace"], _) -> runOne "trace"
    _              -> loop
