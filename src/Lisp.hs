{-# LANGUAGE ExistentialQuantification #-}

module Lisp
  ( primitiveBindings
  , evalString
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.IORef
import           Repl                          (Env, IOThrowsExcept,
                                                LanguageError (..),
                                                ThrowsExcept, bindVars,
                                                defineVar, getVar, liftThrows,
                                                nullEnv, runIOThrows, setVar)
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsExcept LispVal)
  | Func { params  :: [String]
         , vararg  :: (Maybe String)
         , body    :: [LispVal]
         , closure :: Env LispVal }

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return (String x)

-- parseAtom parses and atom, which is a letter or symbol,
-- followed by any number of letters, digits, or symbols.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

parseList :: Parser LispVal
parseList = liftM List (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return (DottedList head tail)

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return (List [Atom "quote", x])

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

readExpr :: String -> ThrowsExcept LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> throwError (Parser err)
    Right val -> return val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal -- The "unwords" function glues together a list of words with spaces.

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++
  unwords (map show args) ++
  (case varargs of
     Nothing  -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"

instance Show LispVal where
  show = showVal

numericBinop ::
     (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsExcept LispVal
numericBinop op [] = throwError (NumArgs 2 0)
numericBinop op singleVal@[_] = throwError (NumArgs 2 1)
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsExcept Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError (TypeMismatch "number" (show (String n)))
        else return (fst (parsed !! 0))
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError (TypeMismatch "number" (show notNum))

boolBinop ::
     (LispVal -> ThrowsExcept a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsExcept LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 (length args)
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsExcept String
unpackStr (String s) = return s
unpackStr (Number s) = return (show s)
unpackStr (Bool s)   = return (show s)
unpackStr notString  = throwError (TypeMismatch "string" (show notString))

unpackBool :: LispVal -> ThrowsExcept Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError (TypeMismatch "boolean" (show notBool))

-- car returns the first element of a list.
car :: [LispVal] -> ThrowsExcept LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError (TypeMismatch "pair" (show badArg))
car badArgList            = throwError (NumArgs 1 (length badArgList))

-- cdr returns what remains of the list after removing the first element.
cdr :: [LispVal] -> ThrowsExcept LispVal
cdr [List (x:xs)]         = return (List xs)
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return (DottedList xs x)
cdr [badArg]              = throwError (TypeMismatch "pair" (show badArg))
cdr badArgList            = throwError (NumArgs 1 (length badArgList))

-- cons constructs lists; it is the inverse of "car" and "cdr".
cons :: [LispVal] -> ThrowsExcept LispVal
cons [x1, List []]            = return (List [x1])
cons [x, List xs]             = return (List (x : xs))
cons [x, DottedList xs xlast] = return (DottedList (x : xs) xlast) -- If the list is a "DottedList", then it should stay a "DottedList", taking into account the improper tail.
cons [x1, x2]                 = return (DottedList [x1] x2) -- Cons of two non-lists results in a "DottedList".
cons badArgList               = throwError (NumArgs 2 (length badArgList))

eqv :: [LispVal] -> ThrowsExcept LispVal
eqv [(Bool arg1), (Bool arg2)] = return (Bool (arg1 == arg2))
eqv [(Number arg1), (Number arg2)] = return (Bool (arg1 == arg2))
eqv [(String arg1), (String arg2)] = return (Bool (arg1 == arg2))
eqv [(Atom arg1), (Atom arg2)] = return (Bool (arg1 == arg2))
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [(List arg1), (List arg2)] =
  return (Bool ((length arg1 == length arg2) && (all eqvPair (zip arg1 arg2))))
  where
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left err         -> False
        Right (Bool val) -> val
eqv [_, _] = return (Bool False)
eqv badArgList = throwError (NumArgs 2 (length badArgList))

data Unpacker =
  forall a. Eq a =>
            AnyUnpacker (LispVal -> ThrowsExcept a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsExcept Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` (const $ return False)

-- equal ignores differences in the type tags and only tests if two values can be interpreted the same.
equal :: [LispVal] -> ThrowsExcept LispVal
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
equal badArgList = throwError $ NumArgs 2 (length badArgList)

-- primitives represent the primitives, which are expected to be found as functions in the Scheme language represented as a list of pairs,
-- containing the key we are to find with "lookup" and the function we are to apply to the arguments.
primitives :: [(String, [LispVal] -> ThrowsExcept LispVal)]
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

makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarArgs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsExcept LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (length params) (length args)
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>=
         evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
        Nothing -> return env

-- eval maps some code to some data.
-- In Lisp, the data types for both code and data are the same, hence eval returns a "LispVal".
eval :: Env LispVal -> LispVal -> IOThrowsExcept LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  eval env pred >>=
  (\x ->
     case x of
       (Bool False) -> eval env alt
       otherwise    -> eval env conseq)
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define":List (Atom var:params):body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params) varargs:body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda":List params:body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda":DottedList params varargs:body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda":varargs@(Atom _):body)) =
  makeVarArgs varargs env [] body
eval env (List (function:args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" (show badForm)

primitiveBindings :: IO (Env LispVal)
primitiveBindings =
  nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

evalString :: (Env LispVal) -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
