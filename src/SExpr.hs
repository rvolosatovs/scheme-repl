module SExpr (readExpr) where

import SParser (LispVal, parseExpr)
import SError (LispError(Parser), ThrowsError)

import Control.Monad.Error
import Text.ParserCombinators.Parsec

-- In order to avoid import cycles this method had to be taken out of the parser file.
-- It works as the final "readExpr" in there, but has error handling.

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError (Parser err)
     Right val -> return val