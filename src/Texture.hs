module Texture (
    loadTex, tryLoadTex
) where

import Graphics

loadTex :: FilePath -> IO TextureObject
loadTex file = do
    tex <- readTexture file
    case tex of
        Left err -> error $ "Could not load " ++ file ++ ": " ++ err
        Right tex' -> do
            generateMipmap' Texture2D --the texture will be active at this point
            textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
            textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
            textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
            return tex'

tryLoadTex :: FilePath -> IO (Either String TextureObject)
tryLoadTex file = do
    tex <- readTexture file
    case tex of
        Right _ -> do
            generateMipmap' Texture2D --the texture will be active at this point
            textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
            textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
            textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
        _ -> return ()
    return tex