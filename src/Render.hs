{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts, DeriveDataTypeable #-}
module Render (
    Renderable(..),

    makeWireframe,
    makeObject,
    drawObject,
) where

import qualified Data.Vector.Storable as V
import Foreign.Ptr(nullPtr)
import Control.Monad.Trans.Class
import Data.Typeable(Typeable)

import qualified Graphics as G
import Math.Mesh
import Types
import Attributes

data Renderable = Renderable { objVAO :: G.VertexArrayObject
                             , objDraw :: IO ()
                             , freeObject :: IO ()
                             , objShader :: G.ShaderProgram
                             }
    deriving Typeable

-- | Make a simple wireframe from a line list
makeWireframe :: VertexAttribs a => V.Vector a -> G.ShaderProgram -> IO Renderable
makeWireframe verts shdr = do
    vertBuf <- bufferVertices verts
    vao <- G.makeVAO $ do
        setVertices vertBuf shdr
    return Renderable { objVAO = vao
                      , objDraw = G.drawArrays G.Lines 0 (fromIntegral (V.length verts))
                      , objShader = shdr
                      , freeObject = do
                            G.deleteObjectNames [vao]
                            deleteVertices vertBuf
                      }

-- | Make an object with indices and attributes
makeObject :: VertexAttribs a => V.Vector a -> V.Vector TriInd -> G.ShaderProgram -> IO Renderable
makeObject verts faces shdr = do
    vertBuf <- bufferVertices verts
    indBuf <- G.bufferIndices faceWords
    vao <- G.makeVAO $ do
        setVertices vertBuf shdr
        G.bindBuffer G.ElementArrayBuffer G.$= Just indBuf
    return Renderable { objVAO = vao
                      , objDraw = G.drawElements G.Triangles (fromIntegral (V.length faceWords)) G.UnsignedInt nullPtr
                      , objShader = shdr
                      , freeObject = do
                            G.deleteObjectNames [vao]
                            deleteVertices vertBuf
                            G.deleteObjectNames [indBuf]
                      }
    where faceWords :: V.Vector G.Word32
          faceWords = V.unsafeCast faces

--Shader
drawObject :: Renderable -> UnifSetter -> DrawFun
drawObject object (UnifSetter unifs) alpha =
    lift $ G.withVAO vao $ do
        G.currentProgram G.$= Just (G.program shdr)
        unifs shdr alpha
        drawIt
    where Renderable {objVAO = vao, objDraw = drawIt, objShader = shdr} = object