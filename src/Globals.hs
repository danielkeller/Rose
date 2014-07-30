-- | Basically StateT for arbitrary global varaibles
module Globals (
    Global(..),
    GlobalsT, Globals,
    get, put,
    modify, gets,
    evalGlobalsT, evalGlobals,
    unwrapGlobalsT,
) where

import Data.Functor.Identity
import Control.Monad
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Class
import Data.Dynamic
import qualified Data.Map as M

data Global a = Global a String
    deriving (Eq, Show)

newtype GlobalsT m a = Globals {unGlobals :: (S.StateT (M.Map String Dynamic) m a) }

type Globals = GlobalsT Identity

instance MonadTrans GlobalsT where
    lift = Globals . lift

instance Monad m => Monad (GlobalsT m) where
    a >>= f = Globals $ unGlobals a >>= unGlobals . f
    return = Globals . return

get :: (Typeable a, Monad m) => Global a -> GlobalsT m a
get (Global initial name) = do
    vars <- Globals S.get
    return $ case M.lookup name vars of
        Nothing -> initial
        Just dyn -> case fromDynamic dyn of
            Just val -> val
            Nothing -> error $ "Name clash for " ++ name ++ " "
                             ++ show (typeOf initial) ++ " vs " ++ show (dynTypeRep dyn)

put :: (Typeable a, Monad m) => Global a -> a -> GlobalsT m ()
put (Global _ name) v = Globals $ S.modify $ M.insert name (toDyn v)

modify :: (Typeable a, Monad m) => Global a -> (a -> a) -> GlobalsT m ()
modify g f = get g >>= (put g . f)

gets :: (Typeable a, Monad m) => Global a -> (a -> b) -> GlobalsT m b
gets g f = liftM f (get g)

evalGlobals :: Globals a -> a
evalGlobals (Globals g) = S.evalState g M.empty

evalGlobalsT :: Monad m => GlobalsT m a -> m a
evalGlobalsT (Globals g) = S.evalStateT g M.empty

unwrapGlobalsT :: Monad m => Globals a -> GlobalsT m a
unwrapGlobalsT (Globals g) = Globals $ (return . S.evalState g) =<< S.get