module Lisp (readExpr) where

import Generic 
  
import Control.Monad
import Control.Monad.Except
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser GenVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return (String x)

-- parseAtom parses and atom, which is a letter or symbol, 
-- followed by any Integer of letters, digits, or symbols.
parseAtom :: Parser GenVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseInteger :: Parser GenVal
parseInteger = liftM (Integer . read) (many1 digit)

parseList :: Parser GenVal
parseList = liftM List (sepBy parseExpr spaces)

parseDottedList :: Parser GenVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return (DottedList head tail)

parseQuoted :: Parser GenVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return (Statement [Atom "quote", x])

parseFunctionDef :: Parser GenVal
parseFunctionDef = do
  w <- parseAtom 
  char '('
  x <- parseAtom
  y <- parseList
  char ')'
  z <- parseExpr
  return (Statement [w, x, y, z])
  
parseVarArgsFunctionDef :: Parser GenVal
parseVarArgsFunctionDef = do
  w <- parseAtom 
  char '('
  x <- parseAtom 
  y <- endBy parseList spaces
  List (z:zs) <- char '.' >> spaces >> parseList
  char ')'
  return (Statement [w, x, List y, List zs, z])
  
parseLambda :: Parser GenVal
parseLambda = do
  x <- parseAtom 
  char '('
  List (y:ys) <- parseList
  char ')'
  return (Statement [x, y, List ys])
  
parseVarArgsLambda :: Parser GenVal
parseVarArgsLambda = do
  x <- parseAtom 
  char '('
  y <- endBy parseExpr spaces
  List (z:zs) <- char '.' >> spaces >> parseList
  char ')'
  return (Statement [x, List y, List zs, z])
  
parseNoArgsLambda :: Parser GenVal
parseNoArgsLambda = do
  List (x1:x2:xs) <- parseList
  return (Statement [x1, List xs, x2])
  
parseStatement :: Parser GenVal
parseStatement = do
  try parseFunctionDef <|> try parseVarArgsFunctionDef <|> try parseLambda <|> try parseVarArgsLambda <|> try parseNoArgsLambda <|> do
      x <- parseAtom 
      y <- parseList
      return (Statement [x, y])

parseExpr :: Parser GenVal
parseExpr =
  parseAtom <|> parseString <|> parseInteger <|> parseQuoted <|> do
    char '('
    x <- try parseStatement <|> try parseList <|> parseDottedList
    char ')'
    return x

readExpr :: String -> ThrowsError GenVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError (Parser err)
    Right val -> return val