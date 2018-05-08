module Generic (GenVal(..), showVal, GenError(..), showError, ThrowsError, trapError, extractValue, Env, nullEnv, IOThrowsError, liftThrows, runIOThrows, runOne, runRepl) where
         
import Control.Monad.Except
import Data.IORef
import Text.Parsec.Error
         
data GenVal
  | Atom String -- Unique identifier for a certain action or value.
  | Statement Atom [GenVal] -- Execution of an action (highly variable interpretation).
  | List [GenVal]
  | DottedList [GenVal] GenVal -- Particular of functional languages.
  | Integer Integer 
  | Double Double
  | String String
  | Bool Bool
  | PrimitiveFunc ([GenVal] -> ThrowsError GenVal)
  | Func { params :: [String]
         , vararg :: (Maybe String)
         , body :: [GenVal]
         , closure :: Env }
 
unwordsList :: [GenVal] -> String
unwordsList = unwords . map showVal -- The "unwords" function glues together a list of words with spaces.
 
showVal :: GenVal -> String
showVal (Atom name) = name
showVal (Statement (Atom name) _) = "(" + name + ") (...)"
showVal (Integer contents) = show contents
showVal (Double contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "<true>" -- Language specific, show placeholder.
showVal (Bool False) = "<false>" -- Language specific, show placeholder.
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++
  unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"
  
instance Show GenVal where
  show = showVal
         
data GenError
  = NumArgs Integer [GenVal]
  | TypeMismatch String GenVal
  | Parser ParseError
  | BadSpecialForm String GenVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  
showError :: GenError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show GenError where
  show = showError
  
-- ThrowsError represents functions that may throw a "GenError" or return a value.
type ThrowsError = Either GenError

-- trapError and extractValue are used in conjunction to extract
-- the String representation of the errors, like so for example: (putStrLn $ extractValue $ trapError)
trapError :: (Show e, MonadError e m) => m String -> m String
trapError = flip catchError (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type Env = IORef [(String, IORef GenVal)]

-- nullEnv returns an empty environment.
-- It needs to be IO Env, because all accesses to IORef's must be sequenced.
nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT GenError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

--- runIOThrows converts errors to strings wrapped in IO monad.
runIOThrows :: IOThrowsError String -> IO String
runIOThrows = (extractValue `fmap`) . runExceptT . trapError

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError GenVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> GenVal -> IOThrowsError GenVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: Env -> String -> GenVal -> IOThrowsError GenVal
defineVar envRef var value = do
  already <- liftIO $ isBound envRef var
  if already
    then setVar envRef var value >> return value
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: Env -> [(String, GenVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, val) = newIORef val >>= return . (,) var

numericBinop ::
     (Integer -> Integer -> Integer) -> [GenVal] -> ThrowsError GenVal
numericBinop op [] = throwError (NumArgs 2 [])
numericBinop op singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinop op params = mapM unpackNum params >>= return . Integer . foldl1 op

unpackNum :: GenVal -> ThrowsError Integer
unpackNum (Integer n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError (TypeMismatch "Integer" (String n))
        else return (fst (parsed !! 0))
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError (TypeMismatch "Integer" notNum)

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

unpackStr :: GenVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Integer s) = return (show s)
unpackStr (Bool s) = return (show s)
unpackStr notString = throwError (TypeMismatch "string" notString)

unpackBool :: GenVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "boolean" notBool)

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

makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarArgs = makeFunc . Just . showVal

apply :: GenVal -> [GenVal] -> IOThrowsError GenVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
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
-- In Lisp, the data types for both code and data are the same, hence eval returns a "GenVal".
eval :: Env -> GenVal -> IOThrowsError GenVal
eval env val@(String _) = return val
eval env val@(Integer _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (Statement (Atom "quote") [Atom val]) = return val
eval env (Statement (Atom "if") [pred, conseq, alt]) =
  eval env pred >>=
  (\x ->
     case x of
       (Bool False) -> eval env alt
       otherwise -> eval env conseq)
eval env (Statement (Atom "set") [Atom var, form]) = eval env form >>= setVar env var
eval env (Statement (Atom "define") [Atom var, form]) =
  eval env form >>= defineVar env var
eval env (Statement (Atom "define") [Atom var, List params, List body) =
  makeNormalFunc env params body >>= defineVar env var
eval env (Statement (Atom "define") [Atom var, List params, List body, Maybe varargs]) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (Statement (Atom "lambda") [List params, List body]) =
  makeNormalFunc env params body
eval env (Statement (Atom "lambda") [List [], List body, Maybe varargs]) =
  makeVarArgs varargs env [] body
eval env (Statement (Atom "lambda") [List params, List body, Maybe varargs]) =
  makeVarArgs varargs env params body
eval env (Statement funtion [List args]) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

-- readPrompt appends a " " to the input, writes it to stdout and reads a line from stdin.
readPrompt :: String -> IO String
readPrompt = (>> getLine) . (>> hFlush stdout) . putStr . (++ " ")

evalString :: (String -> ThrowsError GenVal) -> Env -> String -> IO String
evalString reader env expr =
  runIOThrows $ liftM show $ (liftThrows $ reader expr) >>= eval env

evalAndPrint :: (String -> ThrowsError GenVal) -> Env -> String -> IO ()
evalAndPrint reader env = (putStrLn =<<) . evalString reader env

untilOneOf_ :: (Eq a, Monad m) => [a] -> m a -> (a -> m ()) -> m ()
untilOneOf_ as prompt action = do
  expr <- prompt
  if elem expr as
    then return ()
    else action expr >> untilOneOf_ as prompt action

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

runOne :: (String -> ThrowsError GenVal) -> String -> IO ()
runOne reader = (primitiveBindings >>=) . flip (evalAndPrint reader)

runRepl :: (String -> ThrowsError GenVal) -> String -> IO ()
runRepl reader p = primitiveBindings >>= untilOneOf_ ["quit", "q", "\EOT"] (readPrompt p) . (evalAndPrint reader)