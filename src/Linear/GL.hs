{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | OpenGL type synonyms
module Linear.GL(
    Vec2, Vec3, Vec4,
    Mat2, Mat3, Mat4,
    CFloat(..),

    Xform(..), position, rotation, scale,
    slide, toMat4,

    perspective
) where

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.C.Types(CFloat(..))
import Linear

type Vec2 = V2 GL.GLfloat
type Vec3 = V3 GL.GLfloat
type Vec4 = V4 GL.GLfloat

type Mat2 = M22 GL.GLfloat
type Mat3 = M33 GL.GLfloat
type Mat4 = M44 GL.GLfloat

type Quat = Quaternion GL.GLfloat

--Floats are not bounded for some reason
instance Bounded CFloat where
    minBound = CFloat (-(1/0))
    maxBound = CFloat (1/0)

instance Bounded a => Bounded (V2 a) where
    minBound = V2 minBound minBound
    maxBound = V2 maxBound maxBound
instance Bounded a => Bounded (V3 a) where
    minBound = V3 minBound minBound minBound
    maxBound = V3 maxBound maxBound maxBound
instance Bounded a => Bounded (V4 a) where
    minBound = V4 minBound minBound minBound minBound
    maxBound = V4 maxBound maxBound maxBound maxBound

data Xform = Xform Vec3 Quat CFloat
    deriving Show

--lenses

position :: Functor f => (Vec3 -> f Vec3) -> Xform -> f (Xform)
position f (Xform pos rot scl) = fmap (\pos' -> Xform pos' rot scl) (f pos)

rotation :: Functor f => (Quat -> f Quat) -> Xform -> f (Xform)
rotation f (Xform pos rot scl) = fmap (\rot' -> Xform pos rot' scl) (f rot)

scale :: Functor f => (CFloat -> f CFloat) -> Xform -> f (Xform)
scale f (Xform pos rot scl) = fmap (\scl' -> Xform pos rot scl') (f scl)

-- | (s)lerp for transforms
slide :: CFloat -> Xform -> Xform -> Xform
slide a (Xform pos rot scl) (Xform pos' rot' scl') =
    Xform (lerp a pos pos') (normalize $ slerp rot rot' a) (scl * (1-a) + scl' * a)

toMat4 :: Xform -> Mat4
toMat4 (Xform pos rot scl) = mkTransformationMat (fromQuaternion rot !!* scl) pos

perspective :: Floating a => a -> a -> a -> a -> M44 a
perspective near far fovx aspect = V4 (V4 (1/tanHalfFovx) 0 0 0)
                                      (V4 0 (aspect/tanHalfFovx) 0 0)
                                      (V4 0 0 ((far+near)*dst) (2*far*near*dst))
                                      (V4 0 0 (-1) 0)
    where tanHalfFovx = tan (fovx / 2)
          dst = 1/(near - far)