module Main where

import           Lisp               (evalString, primitiveBindings)
import           Repl               (runOne, runRepl)
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0         -> runRepl primitiveBindings evalString "Lisp λ"
    1         -> runOne primitiveBindings evalString $ args !! 0
    otherwise -> putStrLn "Program takes at most 1 argument"
