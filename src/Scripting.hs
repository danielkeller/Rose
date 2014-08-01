module Scripting (
    ReplThread,
    startReplThread,
    tryRunRepl,
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
import Control.Monad.Trans.Class (lift)

-- much of this module is copied from husk scheme's interpreter

data ReplThread = ReplThread {replIn :: MVar String, replOut :: MVar String}

startReplThread :: IO (ReplThread, Env)
startReplThread = do
    replThread <- ReplThread <$> newEmptyMVar <*> newEmptyMVar
    env <- r5rsEnv
    showBanner
    let loop = do
          input <- HL.getInputLine "@> "
          case input of
              Nothing -> return ()
              Just "" -> return ()
              Just str -> do full <- getMultiLine str
                             lift $ putMVar (replIn replThread) full
                             output <- lift $ takeMVar (replOut replThread)
                             HL.outputStrLn output
          loop
    let settings = HL.Settings (completeScheme env) Nothing True
    _ <- forkIO $ HL.runInputT settings loop
    return (replThread, env)

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

tryRunRepl :: ReplThread -> Env -> IO ()
tryRunRepl replThread env = do
    maybeCode <- tryTakeMVar (replIn replThread)
    case maybeCode of
        Nothing -> return ()
        Just code -> putMVar (replOut replThread) =<< evalString env code

-- |Auto-complete using scheme symbols
completeScheme :: Env -> (String, String) 
               -> IO (String, [HL.Completion])
completeScheme _ (lnL@(')':_), _) = do
  let cOpen  = countLetters '(' lnL
      cClose = countLetters ')' lnL
  if cOpen > cClose
   then return (lnL, [HL.Completion ")" ")" False])
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