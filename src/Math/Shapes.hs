{-# LANGUAGE FlexibleInstances #-}
module Math.Shapes (
    -- shapes
    Tri(..), Box(..),
    -- collision
    Collide(), intersects,
    -- rando operations
    boxIntersect, centroid
) where

import Linear hiding (trace)
import Linear.GL
import Control.Lens.Getter ((^.))

-- | Pattern match convention: (Tri q r s)
data Tri = Tri Vec3 Vec3 Vec3
    deriving Show

-- | Pattern match convention: (Box a b)
-- (Box a b) is bounded on the interval [a, b)
data Box = Box Vec3 Vec3 --i'm pretty sure this is right
    deriving Show

--TODO: Do we need a line segment type that specifies openness and closedness?
--TODO: Should axes return self-projections as an optimization?
--TODO: Should this be * -> * over the vector type, ie: axes :: a v -> [v]

-- | Any primitive collidable in 3D
class Collide a where
    -- | return all potentially separating axes, normalized
    axes :: a -> [Vec3]
    -- | project the shape onto the normalized axis, giving a 1D line segment
    project :: a -> Vec3 -> (CFloat, CFloat)

intersects :: (Collide a, Collide b) => a -> b -> Bool
intersects a b = {-# SCC "intersects" #-} isOverlap `all` axs
    where axs = {-# SCC "intersects.axs" #-} axes a ++ axes b
          --do the line segments overlap?
          overlaps (l, r) (l', r') = {-# SCC "intersects.overlaps" #-} l' <= r && r' >= l
          isOverlap ax = {-# SCC "intersects.isOverlap" #-} project a ax `overlaps` project b ax

{-# SPECIALIZE intersects :: Box -> Tri -> Bool #-}

boxIntersect :: Box -> Box -> Box
boxIntersect (Box a b) (Box a' b') = liftI2 max a a' `Box` liftI2 min b b'

centroid :: Tri -> Vec3
centroid (Tri a b c) = (a ^+^ b ^+^ c) ^/ 3

instance Collide Vec3 where
    axes _ = []
    project v ax = (v `dot` ax, v `dot` ax)

instance Collide Tri where
    --face normal, and edge normals
    {-# INLINE axes #-}
    axes (Tri q r s) = {-# SCC "Tri.axes" #-} map normalize (face : edges)
        where face = {-# SCC "Tri.axes.face" #-} (q ^-^ r) `cross` (s ^-^ r)
              edges ={-# SCC "Tri.axes.edges" #-} map (face `cross`) [q ^-^ r, r ^-^ s, s ^-^ q]

    {-# INLINE project #-}
    project (Tri q r s) ax = {-# SCC "Tri.project" #-} (minimum projs, maximum projs)
        where projs = {-# SCC "Tri.project.projs" #-} map (dot ax) [q, r, s]

--AABBs always have the same axes, obviously
{-# RULES "axes/AABB" forall a b. axes (a::Box) ++ axes (b::Box) = axes a #-}

instance Collide Box where
    {-# INLINE axes #-}
    axes _ = {-# SCC "Box.axes" #-} basis --it's axis aligned!

    {-# INLINE project #-}
    project (Box a b) ax@(V3 x y z) = {-# SCC "Box.project" #-} (min l r, max l r)
        --there are 4 possible pairs of point depending on the direction of ax
              --partially apply just x and y to the constructor in c' and d'
        where (c', d') | x*y > 0 = {-# SCC "Box.project.(c',d')" #-} (V3 (a^._x) (a^._y), V3 (b^._x) (b^._y)) --same x and y
                       | otherwise = {-# SCC "Box.project.(c',d')" #-} (V3 (a^._x) (b^._y), V3 (b^._x) (a^._y)) --different x and y
              (c, d)   | x*z > 0 = {-# SCC "Box.project.(c,d)" #-} (c' (a^._z), d' (b^._z)) --same x and z
                       | otherwise = {-# SCC "Box.project.(c,d)" #-} (c' (b^._z), d' (a^._z)) --different x and z
              (l, r) = {-# SCC "Box.project.(l,r)" #-} (c `dot` ax, d `dot` ax)