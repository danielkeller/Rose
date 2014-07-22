{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts #-}
module Render (
    Renderable(..),

    Pos,
    makeWireframe,
    makeObject,
    drawObject,
) where

import qualified Data.Vector.Storable as V
import Foreign.Ptr(nullPtr)
import Data.Vinyl
import Data.Vinyl.Universe

import Graphics
import Math.Mesh
import Types

data Renderable = Renderable { objMesh :: Mesh
                             , objVAO :: VertexArrayObject
                             , objDraw :: IO ()
                             , freeObject :: IO ()
                             , objShader :: ShaderProgram
                             }

type Pos = SField ("position" ::: Vec3)
pos :: Pos
pos = SField

--type ViableVertex t = (HasFieldNames t, HasFieldSizes t, HasFieldDims t,
--                       HasFieldGLTypes t, V.Storable t)

-- | Make a simple wireframe from a line list
makeWireframe :: (ViableVertex (PlainFieldRec rs), BufferSource (V.Vector (PlainFieldRec rs)), Pos `IElem` rs)
              => V.Vector (PlainFieldRec rs) -> ShaderProgram -> IO Renderable
makeWireframe verts shdr = do
    vertBuf <- bufferVertices verts
    vao <- makeVAO $ do
        enableVertices' shdr vertBuf
        bindVertices vertBuf
    return Renderable { objMesh = mesh
                      , objVAO = vao
                      , objDraw = drawArrays Lines 0 (fromIntegral (V.length verts))
                      , objShader = shdr
                      , freeObject = do
                            deleteObjectNames [vao]
                            deleteVertices vertBuf
                      }
    where mesh = error "Please do not use this :)"

-- | Make an object with indices and attributes
makeObject :: (ViableVertex (PlainFieldRec rs), BufferSource (V.Vector (PlainFieldRec rs)), Pos `IElem` rs)
              => V.Vector (PlainFieldRec rs) -> V.Vector TriInd -> ShaderProgram -> IO Renderable
makeObject verts faces shdr = do
    vertBuf <- bufferVertices verts
    indBuf <- bufferIndices faceWords
    vao <- makeVAO $ do
        enableVertices' shdr vertBuf --this doesn't complain about extra attributes
        bindVertices vertBuf
        bindBuffer ElementArrayBuffer $= Just indBuf
    return Renderable { objMesh = mesh
                      , objVAO = vao
                      , objDraw = drawElements Triangles (fromIntegral (V.length faceWords)) UnsignedInt nullPtr
                      , objShader = shdr
                      , freeObject = do
                            deleteObjectNames [vao]
                            deleteVertices vertBuf
                            deleteObjectNames [indBuf]
                      }
    where mesh = undefined --Mesh (V.map (rGet pos) verts) faces
          faceWords :: V.Vector Word32
          faceWords = V.unsafeCast faces

--Shader
drawObject :: Renderable -> UnifSetter -> DrawFun
drawObject object unifs alpha =
    withVAO vao $ do
        currentProgram $= Just (program shdr)
        unifs shdr alpha
        drawIt
    where Renderable {objVAO = vao, objDraw = drawIt, objShader = shdr} = object