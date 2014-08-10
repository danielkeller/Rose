{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SchemeInterface (
    engineEnv
) where

import qualified Graphics.UI.GLFW as GLFW
import Language.Scheme.Variables
import Language.Scheme.Types
import Language.Scheme.Core (evalAndPrint)
import qualified Graphics as G
import Data.Typeable (Typeable)

import Texture
import Scripting
import Wavefront
import Math.Mesh (Mesh)
import Render (Renderable)

deriving instance Typeable G.TextureObject
instance Scriptable G.TextureObject where
deriving instance Typeable G.ShaderProgram
instance Scriptable G.ShaderProgram where
deriving instance Typeable Mesh
instance Scriptable Mesh where
deriving instance Typeable Renderable
instance Scriptable Renderable where

engineEnv :: GLFW.Window -> Env -> IO Env
engineEnv wnd env = do
    env' <- extendEnv env $ map domakeFunc
        [ ("quit", script (GLFW.setWindowShouldClose wnd True))
        , ("load-texture", script tryLoadTex)
        , ("load-shader", script G.simpleShaderProgram)
        , ("load-wavefront", script loadWavefront)]
    -- use evalString to print error messages for debugging
    _ <- evalAndPrint env' "(load \"scripts/library.scm\")"
    return env'
    where domakeFunc (var, func) = ((varNamespace, var), func)