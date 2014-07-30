{-# LANGUAGE DeriveDataTypeable #-}
module Types (
    DrawFun,
    UnifSetter(..),
    dt,
) where

import Graphics (ShaderProgram, GLfloat)
import Globals
import Data.Typeable(Typeable)

type DrawFun = GLfloat -> GlobalsT IO ()
newtype UnifSetter = UnifSetter {unifSetter :: ShaderProgram -> GLfloat -> IO ()}
    deriving Typeable

dt :: Fractional a => a
dt = 0.03