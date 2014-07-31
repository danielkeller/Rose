module Object where

import Data.List ((\\))
import qualified Data.Set as S

newtype Object = Object Int
    deriving (Eq, Ord, Show)

instance Enum Object where
    toEnum = Object
    fromEnum (Object i) = i

type Scene = S.Set Object

emptyScene :: Scene
emptyScene = S.empty

newObject :: Scene -> (Scene, Object)
newObject scene = (S.insert obj scene, obj)
    where obj = head $ [Object 0 ..] \\ S.toAscList scene -- lowest missing object number