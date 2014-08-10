{-# LANGUAGE FlexibleInstances, DefaultSignatures, OverlappingInstances, ScopedTypeVariables #-}
module Scripting (
    ReplThread,
    startReplThread,
    tryRunRepl,
    Scriptable(), --only allow opaque values
    script, unscript,
) where

import Language.Scheme.Core
import Language.Scheme.Util
import Language.Scheme.Types
import Language.Scheme.Variables (recExportsFromEnv)
import qualified System.Console.Haskeline as HL
import Control.Concurrent
import Control.Applicative
import Data.List
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Dynamic
import Control.Monad.Error

import Linear.GL

-- much of this module is copied from husk scheme's interpreter

data ReplThread = ReplThread {replIn :: MVar String, replOut :: MVar String, replEnv :: Env}

startReplThread :: (Env -> IO Env) -> IO ReplThread
startReplThread extender = do
    env <- extender =<< r5rsEnv
    replThread <- ReplThread <$> newEmptyMVar <*> newEmptyMVar <*> pure env
    showBanner
    let loop = do
          input <- HL.getInputLine "@> "
          case input of
              Nothing -> return ()
              Just str | all isSpace str -> return ()
                       | otherwise -> do
                            full <- getMultiLine str
                            lift $ putMVar (replIn replThread) full
                            output <- lift $ takeMVar (replOut replThread)
                            HL.outputStrLn output
          loop
    let settings = HL.Settings (completeScheme env) Nothing True
    _ <- forkIO $ HL.runInputT settings loop --FIXME: completion for engine symbols
    return replThread

    where another ls = cOpen > cClose
              where cOpen  = countLetters '(' ls
                    cClose = countLetters ')' ls
              
          getMultiLine previous = do
              if another previous
                then do
                  mb_input <- HL.getInputLine "@  "
                  case mb_input of
                    Nothing -> return previous
                    Just input -> getMultiLine $ previous ++ " " ++ input
                else return previous

tryRunRepl :: ReplThread -> IO ()
tryRunRepl replThread = do
    maybeCode <- tryTakeMVar (replIn replThread)
    case maybeCode of
        Nothing -> return ()
        Just code -> putMVar (replOut replThread) =<< evalString (replEnv replThread) code

-- |Auto-complete using scheme symbols
completeScheme :: Env -> (String, String) 
               -> IO (String, [HL.Completion])
completeScheme _ (lnL@(')':_), _) = do
  let cOpen  = countLetters '(' lnL
      cClose = countLetters ')' lnL
  if cOpen > cClose
   then return (lnL, [HL.Completion ")" ")" False]) --doesn't do multi line parens right
   else return (lnL, [])
completeScheme env (lnL, lnR) = complete $ reverse $ readAtom lnL
 where
  complete ('"' : _) = do
    -- Special case, inside a string it seems more
    -- useful to autocomplete filenames
    HL.completeFilename (lnL, lnR)


  complete pre = do
   -- Get list of possible completions from ENV
   xps <- recExportsFromEnv env
   let allDefs = xps ++ specialForms
   let allDefs' = filter (\ (Atom a) -> isPrefixOf pre a) allDefs
   let comps = map (\ (Atom a) -> HL.Completion a a False) allDefs'

   -- Get unused portion of the left-hand string
   let unusedLnL = fromMaybe lnL (stripPrefix (reverse pre) lnL)
   return (unusedLnL, comps)

  -- Not loaded into an env, so we need to list them here
  specialForms = map Atom [ 
       "define"  
     , "define-syntax" 
     , "expand"
     , "hash-table-delete!"
     , "hash-table-set!"
     , "if"
     , "lambda"
     , "let-syntax" 
     , "letrec-syntax" 
     , "quote"
     , "set!"
     , "set-car!"
     , "set-cdr!"
     , "string-set!"
     , "vector-set!"]

  -- Read until the end of the current symbol (atom), if there is one.
  -- There is also a special case for files if a double-quote is found.
  readAtom (c:cs)
    | c == '"' = ['"'] -- Save to indicate file completion to caller
    | c == '(' = []
    | c == '[' = []
    | isSpace c = []
    | otherwise = (c : readAtom cs)
  readAtom [] = []

--facilities for automagically moving things between Haskell and Scheme

class Scriptable a where
    typeName :: a -> String
    default typeName :: Typeable a => a -> String
    typeName a = "(Haskell " ++ show (typeOf a) ++ ")"

    script :: a -> LispVal
    default script :: Typeable a => a -> LispVal
    script = Opaque . toDyn

    unscript' :: LispVal -> Maybe a
    default unscript' :: Typeable a => LispVal -> Maybe a
    unscript' (Opaque a) = fromDynamic a 
    unscript' _ = Nothing

    -- Number of args is already checked
    -- the default is suitable for values
    scriptFn' :: a -> [LispVal] -> IOThrowsError LispVal
    scriptFn' v [] = return (script v)
    scriptFn' _ _ = error "Bug in ScriptableFn"

    numArgs :: a -> Int
    numArgs _ = 0

unscript :: forall a. Scriptable a => LispVal -> IOThrowsError a
unscript arg = case unscript' arg of
    Just a -> return a
    Nothing -> throwError $ TypeMismatch (typeName (undefined :: a)) arg

scriptFn :: Scriptable a => a -> LispVal
scriptFn = CustFunc . help
    where help f args
              | length args == expected = scriptFn' f args
              | otherwise = throwError $ NumArgs (Just $ fromIntegral expected) args
              where expected = numArgs f

instance Scriptable String where
    typeName _ = "string"
    script = String
    unscript' (String s) = Just s
    unscript' _ = Nothing

instance Scriptable CFloat where
    typeName _ = "float"
    script = Float . realToFrac
    --support all numeric types, in the spirit of Scheme
    unscript' (Float a) = Just (realToFrac a)
    unscript' (Rational a) = Just (fromRational a)
    unscript' (Number a) = Just (fromInteger a)
    unscript' _ = Nothing

instance Scriptable Vec3 where
    typeName _ = "(float float float)"
    script (V3 x y z) = List [script x, script y, script z]
    unscript' v = do
        let (List [x, y, z]) = v  -- remember, fail = const Nothing
        x' <- unscript' x
        y' <- unscript' y
        z' <- unscript' z
        Just (V3 x' y' z')

instance Scriptable Quat where
    typeName _ = "(float float float float)"
    script (Quaternion w (V3 x y z)) = List [script w, script x, script y, script z]
    unscript' v = do
        let (List [w, x, y, z]) = v  -- remember, fail = const Nothing
        w' <- unscript' w
        x' <- unscript' x
        y' <- unscript' y
        z' <- unscript' z
        Just (Quaternion w' (V3 x' y' z'))

instance Scriptable Xform where
    typeName _ = "((float float float) (float float float float) float)"
    script (Xform p r s) = List [script p, script r, script s]
    unscript' v = do
        let (List [p, r, s]) = v
        p' <- unscript' p
        r' <- unscript' r
        s' <- unscript' s
        Just (Xform p' r' s')

instance Scriptable () where
    typeName _ = "null"
    script _ = nullLisp
    unscript' _ = Just ()

instance (Scriptable a, Scriptable b) => Scriptable (a, b) where
    typeName _ = "(" ++ typeName (undefined :: a) ++ " " ++ typeName (undefined :: b) ++ ")"
    script (a, b) = List [script a, script b]
    unscript' v = do 
        let (List [a, b]) = v
        a' <- unscript' a
        b' <- unscript' b
        Just (a', b')

instance Scriptable a => Scriptable (IO a) where
    typeName _ = "function"
    script = scriptFn
    scriptFn' io [] = lift $ script <$> io
    scriptFn' _ _ = error "Bug in ScriptableFn"
    numArgs _ = 0
    unscript' = undefined

instance Scriptable a => Scriptable (IO (Either String a)) where
    typeName _ = "function"
    script = scriptFn
    scriptFn' io [] = ErrorT $ either (Left . Default) (Right . script) <$> io
    scriptFn' _ _ = error "Bug in ScriptableFn"
    numArgs _ = 0
    unscript' = undefined

instance (Scriptable a, Scriptable b) => Scriptable (a -> b) where
    typeName _ = "function"
    script = scriptFn
    numArgs f = numArgs (f undefined) + 1
    scriptFn' f (a:as) = do val <- unscript a
                            scriptFn' (f val) as
    scriptFn' _ _ = error "Bug in ScriptableFn"
    unscript' = undefined