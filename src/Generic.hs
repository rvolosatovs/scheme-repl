module Generic
  ( GenVal(..)
  , showVal
  , GenError(..)
  , showError
  , ThrowsError
  , trapError
  , extractValue
  , Env
  , nullEnv
  , IOThrowsError
  , liftThrows
  , runIOThrows
  , unpackNum
  , unpackStr
  , unpackBool
  , runOne
  , runRepl
  ) where

import Control.Monad.Except
import Data.IORef
import System.IO
import Text.Parsec.Error

data GenVal
  = Atom String -- Unique identifier for a certain action or value.
  | Statement [GenVal] -- Execution of an action (highly variable interpretation).
  | List [GenVal]
  | DottedList [GenVal]
               GenVal -- Particular of functional languages.
  | Integer Integer
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
showVal (Statement ((Atom name):body)) =
  "(" ++ name ++ ") (" ++ showVal (List body) ++ ")"
showVal (Integer contents) = show contents
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
  = NumArgs Integer
            [GenVal]
  | TypeMismatch String
                 GenVal
  | Parser ParseError
  | BadSpecialForm String
                   GenVal
  | NotFunction String
                String
  | UnboundVar String
               String
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

unpackNum :: GenVal -> ThrowsError Integer
unpackNum (Integer n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError (TypeMismatch "Integer" (String n))
        else return (fst (parsed !! 0))
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError (TypeMismatch "Integer" notNum)

unpackStr :: GenVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Integer s) = return (show s)
unpackStr (Bool s) = return (show s)
unpackStr notString = throwError (TypeMismatch "string" notString)

unpackBool :: GenVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "boolean" notBool)

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
eval env val@(List _) = return val
eval env val@(DottedList _ _) = return val
eval env (Atom id) = getVar env id
eval env (Statement [Atom "quote", val]) = return val
eval env (Statement [Atom "if", pred, conseq, alt]) =
  eval env pred >>=
  (\x ->
     case x of
       (Bool False) -> eval env alt
       otherwise -> eval env conseq)
eval env (Statement [Atom "set", Atom var, form]) =
  eval env form >>= setVar env var
eval env (Statement [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (Statement [Atom "define", Atom var, List params, List body]) =
  makeNormalFunc env params body >>= defineVar env var
eval env (Statement [Atom "define", Atom var, List params, List body, varargs]) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (Statement [Atom "lambda", List params, List body]) =
  makeNormalFunc env params body
eval env (Statement [Atom "lambda", List params, List body, varargs]) =
  makeVarArgs varargs env params body
eval env (Statement [function, List args]) = do
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

primitiveBindings :: [(String, [GenVal] -> ThrowsError GenVal)] -> IO Env
primitiveBindings primitives =
  nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

runOne ::
     (String -> ThrowsError GenVal)
  -> [(String, [GenVal] -> ThrowsError GenVal)]
  -> String
  -> IO ()
runOne reader primitives =
  ((primitiveBindings primitives) >>=) . flip (evalAndPrint reader)

runRepl ::
     (String -> ThrowsError GenVal)
  -> [(String, [GenVal] -> ThrowsError GenVal)]
  -> String
  -> IO ()
runRepl reader primitives p =
  (primitiveBindings primitives) >>=
  untilOneOf_ ["quit", "q", "\EOT"] (readPrompt p) . (evalAndPrint reader)
