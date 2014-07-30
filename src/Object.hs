{-# LANGUAGE DeriveDataTypeable #-}
module Object where

import Data.List ((\\))
import qualified Data.Set as S
import Data.Typeable(Typeable)

newtype Object = Object Int
    deriving (Eq, Ord, Show, Typeable)

instance Enum Object where
    toEnum = Object
    fromEnum (Object i) = i

type Scene = S.Set Object

emptyScene = S.empty

newObject :: Scene -> (Scene, Object)
newObject scene = (S.insert obj scene, obj)
    where obj = head $ [Object 0 ..] \\ S.toAscList scene -- lowest missing object number