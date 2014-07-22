module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW

import Window
import Wavefront

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()

main :: IO ()
main = withWindow help
    where help wnd = do
              GLFW.setKeyCallback wnd (Just keyCB)
              return (const (return ()))