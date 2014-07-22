module Types (
    DrawFun,
    UnifSetter,
) where

import Graphics (ShaderProgram)

type DrawFun = Float -> IO ()
type UnifSetter = ShaderProgram -> DrawFun