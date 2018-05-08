module Repl
  ( Env
  , IOThrowsExcept
  , LanguageError(..)
  , ThrowsExcept
  , bindVars
  , defineVar
  , getVar
  , liftThrows
  , nullEnv
  , runIOThrows
  , runOne
  , runRepl
  , setVar
  ) where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

data LanguageError
  = NumArgs Int
            Int
  | TypeMismatch String
                 String
  | Parser ParseError
  | BadSpecialForm String
                   String
  | NotFunction String
                String
  | UnboundVar String
               String
  | Default String

showError :: (LanguageError) -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found " ++ show found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LanguageError where
  show = showError

-- ThrowsExcept represents functions that may throw LanguageError or return a value.
type ThrowsExcept = Either LanguageError

-- trapExcept and extractValue are used in conjunction to extract
-- the String representation of the errors, like so for example: (putStrLn $ extractValue $ trapExcept)
trapExcept :: (Show e, MonadError e m) => m String -> m String
trapExcept = flip catchError (return . show)

extractValue :: (ThrowsExcept a) -> a
extractValue (Right val) = val

type Env a = IORef [(String, IORef a)]

-- nullEnv returns an empty environment.
-- It needs to be IO Env, because all accesses to IORef's must be sequenced.
nullEnv :: IO (Env a)
nullEnv = newIORef []

type IOThrowsExcept = ExceptT LanguageError IO

liftThrows :: ThrowsExcept a -> IOThrowsExcept a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

--- runIOThrows converts errors to strings wrapped in IO monad.
runIOThrows :: (IOThrowsExcept String) -> IO String
runIOThrows = (extractValue `fmap`) . runExceptT . trapExcept

isBound :: (Env a) -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: (Env a) -> String -> (IOThrowsExcept a)
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: (Env a) -> String -> a -> (IOThrowsExcept a)
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: (Env a) -> String -> a -> (IOThrowsExcept a)
defineVar envRef var value = do
  already <- liftIO $ isBound envRef var
  if already
    then setVar envRef var value >> return value
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: (Env a) -> [(String, a)] -> IO (Env a)
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, val) = newIORef val >>= return . (,) var

-- readPrompt appends a " " to the input, writes it to stdout and reads a line from stdin.
readPrompt :: String -> IO String
readPrompt = (>> getLine) . (>> hFlush stdout) . putStr . (++ " ")

evalAndPrint :: ((Env a) -> String -> IO String) -> (Env a) -> String -> IO ()
evalAndPrint evalString env = (putStrLn =<<) . evalString env

untilOneOf_ :: (Eq a, Monad m) => [a] -> m a -> (a -> m ()) -> m ()
untilOneOf_ as prompt action = do
  expr <- prompt
  if elem expr as
    then return ()
    else action expr >> untilOneOf_ as prompt action

runOne :: IO (Env a) -> ((Env a) -> String -> IO String) -> String -> IO ()
runOne initEnv evalString s = do
  env <- initEnv
  evalAndPrint evalString env s

runRepl :: IO (Env a) -> ((Env a) -> String -> IO String) -> String -> IO ()
runRepl initEnv evalString p = do
  env <- initEnv
  untilOneOf_ ["quit", "q", "\EOT"] (readPrompt p) (evalAndPrint evalString env)
