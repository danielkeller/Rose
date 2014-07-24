{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts #-}
module Render (
    Renderable(..),

    makeWireframe,
    makeObject,
    drawObject,
) where

import qualified Data.Vector.Storable as V
import Foreign.Ptr(nullPtr)

import qualified Graphics as G
import Math.Mesh
import Types
import Attributes

data Renderable = Renderable { objMesh :: Mesh
                             , objVAO :: G.VertexArrayObject
                             , objDraw :: IO ()
                             , freeObject :: IO ()
                             , objShader :: G.ShaderProgram
                             }

-- | Make a simple wireframe from a line list
makeWireframe :: VertexAttribs a => V.Vector a -> G.ShaderProgram -> IO Renderable
makeWireframe verts shdr = do
    vertBuf <- bufferVertices verts
    vao <- G.makeVAO $ do
        setVertices vertBuf shdr
    return Renderable { objMesh = mesh
                      , objVAO = vao
                      , objDraw = G.drawArrays G.Lines 0 (fromIntegral (V.length verts))
                      , objShader = shdr
                      , freeObject = do
                            G.deleteObjectNames [vao]
                            deleteVertices vertBuf
                      }
    where mesh = error "Please do not use this :)"

-- | Make an object with indices and attributes
makeObject :: VertexAttribs a => V.Vector a -> V.Vector TriInd -> G.ShaderProgram -> IO Renderable
makeObject verts faces shdr = do
    vertBuf <- bufferVertices verts
    indBuf <- G.bufferIndices faceWords
    vao <- G.makeVAO $ do
        setVertices vertBuf shdr
        G.bindBuffer G.ElementArrayBuffer G.$= Just indBuf
    return Renderable { objMesh = mesh
                      , objVAO = vao
                      , objDraw = G.drawElements G.Triangles (fromIntegral (V.length faceWords)) G.UnsignedInt nullPtr
                      , objShader = shdr
                      , freeObject = do
                            G.deleteObjectNames [vao]
                            deleteVertices vertBuf
                            G.deleteObjectNames [indBuf]
                      }
    where mesh = Mesh (V.map position verts) faces
          faceWords :: V.Vector G.Word32
          faceWords = V.unsafeCast faces

--Shader
drawObject :: Renderable -> UnifSetter -> DrawFun
drawObject object unifs alpha =
    G.withVAO vao $ do
        G.currentProgram G.$= Just (G.program shdr)
        unifs shdr alpha
        drawIt
    where Renderable {objVAO = vao, objDraw = drawIt, objShader = shdr} = object