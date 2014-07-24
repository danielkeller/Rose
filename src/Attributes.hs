{-# LANGUAGE ScopedTypeVariables #-}
module Attributes (
    AttribProperties(..),
    VertexAttribs(..),

    BufferedVerts,
    bufferVertices, setVertices, deleteVertices,
) where

import Foreign.Storable
import qualified Data.Vector.Storable as V
import qualified Data.Map as M

import Graphics

data AttribProperties = AttribProperties { attrib :: String
                                         -- , intHandling :: IntegerHandling
                                         , numComponents :: NumComponents
                                         , dataType :: DataType
                                         , offset :: Int
                                         }

class Storable a => VertexAttribs a where
    schema :: a -> [AttribProperties]

newtype BufferedVerts a = BufferedVerts { unBufferedVerts :: BufferObject }

bufferVertices :: VertexAttribs a => V.Vector a -> IO (BufferedVerts a)
bufferVertices verts = fmap BufferedVerts (fromVector ArrayBuffer verts)

--minimal API for now
--TODO: check the shader
setVertices :: forall a. VertexAttribs a => BufferedVerts a -> ShaderProgram -> IO ()
setVertices verts shader = do
    bindBuffer ArrayBuffer $= Just (unBufferedVerts verts)
    mapM_ enableOne (schema (undefined :: a))
    where enableOne prop = do
              enableAttrib shader attribName
              setAttrib shader attribName ToNormalizedFloat $
                  VertexArrayDescriptor (numComponents prop) (dataType prop) stride (offsetPtr (offset prop))
              where (_, shdrType) = attribs shader M.! attrib prop
                    attribName = attrib prop
                    stride = fromIntegral $ sizeOf (undefined :: a)

deleteVertices :: BufferedVerts a -> IO ()
deleteVertices = deleteObjectName . unBufferedVerts