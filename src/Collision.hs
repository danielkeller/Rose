module Collision (
    drawAABB
) where

import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr(castPtr)
import Control.Applicative
import Control.Monad(zipWithM_)
import Linear

import qualified Graphics as G
import Attributes
import Math.BVH
import Math.Shapes
import Render

-- | visualize the AABB
drawAABB :: AABB -> IO Renderable
drawAABB aabb = do
    shdr <- G.simpleShaderProgram "assets/color.vert" "assets/color.frag"
    makeWireframe verts shdr
    where verts = bvhWireframe aabb

data AABBVert = AABBVert G.Vec3 G.Vec3

instance Storable AABBVert where
    sizeOf _ = 6 * (sizeOf (undefined :: G.CFloat))
    alignment _ = alignment (undefined :: G.CFloat)
    peek ptr = do [x, y, z, r, g, b] <- mapM (peekElemOff (castPtr ptr)) [0..5]
                  return $ AABBVert (V3 x y z) (V3 r g b)
    poke ptr (AABBVert (V3 x y z) (V3 r g b)) =
            zipWithM_ (pokeElemOff (castPtr ptr)) [0..5] [x, y, z, r, g, b]

instance VertexAttribs AABBVert where
    schema _ = [ AttribProperties "position" 3 G.Float 0
               , AttribProperties "color"    3 G.Float (3*floatSize)]
        where floatSize = sizeOf (undefined :: G.CFloat)

bvhWireframe :: AABB -> V.Vector AABBVert
bvhWireframe b = help b 0
    where help aabb depth = case aabb of
            Leaf box _ -> buildVector (boxWf box)
            Node box l r -> help l (depth + 1) V.++ help r (depth + 1) V.++ buildVector (boxWf box)
            where buildVector points = V.fromList $ zipWith AABBVert points (repeat (rainbow depth 6))
          boxWf (Box (V3 lx ly lz) (V3 hx hy hz)) =
                 (V3  <$> [lx, hx] <*> [ly, hy] <*> [lz, hz])
              ++ (xzy <$> [lx, hx] <*> [lz, hz] <*> [ly, hy])
              ++ (yzx <$> [ly, hy] <*> [lz, hz] <*> [lx, hx])
          -- because of the way applicative works, the lines are in the axis of the last argument
          xzy x z y = V3 x y z
          yzx y z x = V3 x y z

rainbow :: G.CFloat -> G.CFloat -> G.Vec3
rainbow pos end = V3 (sine 0) (sine 2) (sine 4)
    where sine phase = sin (2 * pi * pos / end + phase) / 2 + 0.5
