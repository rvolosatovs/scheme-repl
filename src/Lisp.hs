{-# LANGUAGE ExistentialQuantification #-}

module Lisp (readExpr, primitives) where

import Generic 
  
import Control.Monad
import Control.Monad.Except
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

----------------------------- Language parser -----------------------------

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
  x <- parseAtom
  spaces
  char '('
  List (y:ys) <- parseList
  char ')'
  spaces
  char '('
  List z <- parseList
  char ')'
  return (Statement [x, y, List ys, List z])
  
parseVarArgsFunctionDef :: Parser GenVal
parseVarArgsFunctionDef = do
  w <- parseAtom 
  spaces
  char '('
  x <- parseAtom
  spaces
  y <- endBy parseList spaces
  List (z:zs) <- char '.' >> spaces >> parseList
  char ')'
  return (Statement [w, x, List y, List zs, z])
  
parseLambda :: Parser GenVal
parseLambda = do
  x <- parseAtom 
  spaces
  char '('
  List (y:ys) <- parseList
  char ')'
  return (Statement [x, y, List ys])
  
parseVarArgsLambda :: Parser GenVal
parseVarArgsLambda = do
  x <- parseAtom 
  spaces
  char '('
  y <- endBy parseExpr spaces
  List (z:zs) <- char '.' >> spaces >> parseList
  char ')'
  return (Statement [x, List y, List zs, z])
  
parseStatement :: Parser GenVal
parseStatement = do
  try parseFunctionDef <|> try parseVarArgsFunctionDef <|> try parseLambda <|> try parseVarArgsLambda <|> do
      List (x:xs) <- parseList
      return (Statement [x, List xs])

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
 
---------------------------- Primitive functions ----------------------------

numericBinop ::
     (Integer -> Integer -> Integer) -> [GenVal] -> ThrowsError GenVal
numericBinop op [] = throwError (NumArgs 2 [])
numericBinop op singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinop op params = mapM unpackNum params >>= return . Integer . foldl1 op

boolBinop ::
     (GenVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [GenVal]
  -> ThrowsError GenVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool
 
-- car returns the first element of a list.
car :: [GenVal] -> ThrowsError GenVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError (TypeMismatch "pair" badArg)
car badArgList = throwError (NumArgs 1 badArgList)

-- cdr returns what remains of the list after removing the first element.
cdr :: [GenVal] -> ThrowsError GenVal
cdr [List (x:xs)] = return (List xs)
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return (DottedList xs x)
cdr [badArg] = throwError (TypeMismatch "pair" badArg)
cdr badArgList = throwError (NumArgs 1 badArgList)

-- cons constructs lists; it is the inverse of "car" and "cdr".
cons :: [GenVal] -> ThrowsError GenVal
cons [x1, List []] = return (List [x1])
cons [x, List xs] = return (List (x : xs))
cons [x, DottedList xs xlast] = return (DottedList (x : xs) xlast) -- If the list is a "DottedList", then it should stay a "DottedList", taking into account the improper tail.
cons [x1, x2] = return (DottedList [x1] x2) -- Cons of two non-lists results in a "DottedList".
cons badArgList = throwError (NumArgs 2 badArgList)

eqv :: [GenVal] -> ThrowsError GenVal
eqv [(Bool arg1), (Bool arg2)] = return (Bool (arg1 == arg2))
eqv [(Integer arg1), (Integer arg2)] = return (Bool (arg1 == arg2))
eqv [(String arg1), (String arg2)] = return (Bool (arg1 == arg2))
eqv [(Atom arg1), (Atom arg2)] = return (Bool (arg1 == arg2))
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [(List arg1), (List arg2)] =
  return (Bool ((length arg1 == length arg2) && (all eqvPair (zip arg1 arg2))))
  where
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left err -> False
        Right (Bool val) -> val
eqv [_, _] = return (Bool False)
eqv badArgList = throwError (NumArgs 2 badArgList)

data Unpacker =
  forall a. Eq a =>
            AnyUnpacker (GenVal -> ThrowsError a)

unpackEquals :: GenVal -> GenVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` (const $ return False)

-- equal ignores differences in the type tags and only tests if two values can be interpreted the same.
equal :: [GenVal] -> ThrowsError GenVal
equal [arg1, arg2] = do
  primitiveEquals <-
    liftM or $
    mapM
      (unpackEquals arg1 arg2)
      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $
    Bool $
    (primitiveEquals ||
     let (Bool x) = eqvEquals
      in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- primitives represent the primitives, which are expected to be found as functions in the Scheme language represented as a list of pairs,
-- containing the key we are to find with "lookup" and the function we are to apply to the arguments.
primitives :: [(String, [GenVal] -> ThrowsError GenVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]