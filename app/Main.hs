module Main where

import           Lisp               (runOne, runRepl)
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0         -> runRepl "Lisp λ"
    1         -> runOne $ args !! 0
    otherwise -> putStrLn "Program takes at most 1 argument"
