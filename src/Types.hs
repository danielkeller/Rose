{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types (
    DrawFun,
    UnifSetter(..),
    dt,
) where

import Data.Monoid
import Graphics (ShaderProgram, GLfloat)

type DrawFun = GLfloat -> IO ()
newtype UnifSetter = UnifSetter (ShaderProgram -> GLfloat -> IO ())

instance Monoid UnifSetter where
    mempty = UnifSetter f
        where f _ _ = return ()
    mappend (UnifSetter l) (UnifSetter r) = UnifSetter f
        where f shdr alpha = l shdr alpha >> r shdr alpha

instance Monoid DrawFun where
    mempty _ = return ()
    mappend l r alpha = l alpha >> r alpha

dt :: Fractional a => a
dt = 0.03