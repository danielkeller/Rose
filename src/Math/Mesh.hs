{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Math.Mesh (
    Mesh(..), TriInd(..),
    PointCloud(..),

    module Data.Collections,
    length, span, sortBy,
    minBoundBoxPt, maxBoundBoxPt
) where

import qualified Prelude as P
import Prelude hiding (map, length, null, foldr, foldl,
                       span, takeWhile, splitAt,
                       minimum, maximum, filter)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Algorithms.Intro as V
import Linear.GL
import Data.Word(Word32)
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative hiding (empty)
import Data.Collections
import Data.Monoid

import Math.Shapes

data TriInd = TriInd Word32 Word32 Word32

instance Storable TriInd where
    sizeOf _ = 3 * sizeOf (0 :: Word32)
    alignment _ = alignment (0 :: Word32)
    peek p = TriInd <$> peek p' <*> peekElemOff p' 1 <*> peekElemOff p' 2
        where p' = castPtr p
    poke p (TriInd a b c) = poke p' a >> pokeElemOff p' 1 b >> pokeElemOff p' 2 c
        where p' = castPtr p

data Mesh = Mesh { meshVerts :: V.Vector Vec3
                 , meshInds :: V.Vector TriInd
                 }

instance Unfoldable Mesh Tri where
    insert (Tri a b c) (Mesh vs is) = Mesh (vs V.++ V.fromList [a, b, c])
                                           (V.snoc is $ TriInd len (len+1) (len+2))
        where len = fromIntegral (V.length vs)
    empty = Mesh V.empty V.empty

instance Collection Mesh Tri where
    filter f m@(Mesh vs is) = Mesh vs (V.filter help is)
        where help ind = f (triAt m ind)

instance Monoid Mesh where
    mempty = empty
    mappend (Mesh vs is) (Mesh vs' is') = Mesh (vs V.++ vs') (is V.++ V.map toEnd is')
        where toEnd (TriInd a b c) = TriInd (a + len) (b + len) (c + len)
              len = fromIntegral (V.length vs)

instance Sequence Mesh Tri where
    take i c = fst (splitAt i c)
    drop i c = snd (splitAt i c)
    splitAt i (Mesh vs is) = (Mesh vs l, Mesh vs r)
        where (l, r) = V.splitAt i is
    reverse (Mesh vs is) = Mesh vs (V.reverse is)
    front m@(Mesh vs is) | null m = Nothing
                         | otherwise = Just (triAt m (V.head is), Mesh vs (V.tail is))
    back m@(Mesh vs is) | null m = Nothing
                        | otherwise = Just (Mesh vs (V.init is), triAt m (V.last is))
    snoc = flip insert

instance Foldable Mesh Tri where
    foldr f i m = help (length m - 1) i
        where help 0 acc = acc
              help idx acc = help (idx - 1) (m ! idx `f` acc)

newtype PointCloud = PointCloud Mesh

instance Foldable PointCloud Vec3 where
    foldr f i (PointCloud m) = help (length m - 1) i
        where help 0 acc = acc
              help idx acc = help (idx - 1) (f a $ f b $ f c $ acc)
                  where ~(Tri a b c) = m ! idx

instance Indexed Mesh Int Tri where
    index idx m@(Mesh _ is) = triAt m (is V.! idx)
    adjust f idx m@(Mesh vs is) = Mesh (vs V.++ V.fromList [a, b, c]) (is V.// [(idx, end)])
        where len = fromIntegral (V.length vs)
              end = TriInd len (len+1) (len+2)
              Tri a b c = f (m ! idx)
    inDomain idx m = idx < length m

triAt :: Mesh -> TriInd -> Tri
triAt (Mesh vs _) (TriInd a b c) = Tri (vs V.! fromIntegral a) (vs V.! fromIntegral b) (vs V.! fromIntegral c)

length :: Mesh -> Int
length = V.length . meshInds

span :: (Tri -> Bool) -> Mesh -> (Mesh, Mesh)
span f m = splitAt (combine (foldrM lastIdx 0 m)) m
    where lastIdx tr idx | f tr = Right (succ idx)
                         | otherwise = Left idx
          combine (Left a) = a
          combine (Right a) = a

--Tris are not Ord because that wouldn't make sense
sortBy :: (Tri -> Tri -> Ordering) -> Mesh -> Mesh
sortBy f m@(Mesh vs is) = Mesh vs (sortVectorBy is)
    where f' indl indr = f (m `triAt` indl) (m `triAt` indr)
          sortVectorBy = V.modify (V.sortBy f')

minBoundBoxPt :: Mesh -> Vec3
minBoundBoxPt mesh = foldl help maxBound (PointCloud mesh)
    where help (V3 mx my mz) (V3 cx cy cz) = V3 (min mx cx) (min my cy) (min mz cz)

maxBoundBoxPt :: Mesh -> Vec3
maxBoundBoxPt mesh = foldl help minBound (PointCloud mesh)
    where help (V3 mx my mz) (V3 cx cy cz) = V3 (max mx cx) (max my cy) (max mz cz)