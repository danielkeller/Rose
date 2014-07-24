-- | Graphics facade
module Graphics (
    module Graphics.Rendering.OpenGL,
    module Graphics.GLUtil,
    module Linear.GL,
    setUniform
) where

import Graphics.Rendering.OpenGL hiding (Shader, Uniform, perspective, position, scale)
import Graphics.GLUtil hiding (setUniform)
import Linear.GL

import qualified Data.Map as M (lookup)

setUniform :: AsUniform a => ShaderProgram -> String -> a -> IO ()
setUniform shdr name = maybe (const (putStrLn warn))
                             (\(u,_) x -> asUniform x u)
                             (M.lookup name $ uniforms shdr)
    where warn = "WARNING: uniform "++name++" is not active"