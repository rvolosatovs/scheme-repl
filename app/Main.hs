module Main where

import           Lisp               (readExpr)
import           Generic            (runOne, runRepl)
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0         -> runRepl readExpr "Lisp λ"
    1         -> runOne readExpr $ args !! 0
    otherwise -> putStrLn "Program takes at most 1 argument"