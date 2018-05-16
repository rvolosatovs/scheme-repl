module Main where

import           Lisp               (primitives, readExpr)
import           Repl               (runOne, runRepl)
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0         -> runRepl readExpr primitives "Lisp λ"
    1         -> runOne readExpr primitives $ args !! 0
    otherwise -> putStrLn "Program takes at most 1 argument"
