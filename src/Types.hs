module Types (
    DrawFun,
    UnifSetter,
    dt,
) where

import Graphics (ShaderProgram, GLfloat)

type DrawFun = GLfloat -> IO ()
type UnifSetter = ShaderProgram -> DrawFun

dt :: Fractional a => a
dt = 0.03