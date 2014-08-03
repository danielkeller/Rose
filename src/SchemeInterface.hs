{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SchemeInterface (
    engineEnv
) where

import qualified Graphics.UI.GLFW as GLFW
import Language.Scheme.Variables
import Language.Scheme.Types
import qualified Graphics as G
import Data.Typeable (Typeable)

import Texture
import Scripting

deriving instance Typeable G.TextureObject
instance Scriptable G.TextureObject where
deriving instance Typeable G.ShaderProgram
instance Scriptable G.ShaderProgram where

engineEnv :: GLFW.Window -> Env -> IO Env
engineEnv wnd env = extendEnv env $ map domakeFunc
    [ ("quit", scriptFn (GLFW.setWindowShouldClose wnd True))
    , ("load-texture", scriptFn tryLoadTex)
    , ("load-shader", scriptFn G.simpleShaderProgram)]
    where domakeFunc (var, func) = ((varNamespace, var), func)