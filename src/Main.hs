module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW
import Linear

import Window
import Wavefront
import qualified Graphics as G
import Types
import Render
import Texture

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()

draw :: G.Mat4 -> [UnifSetter] -> [Renderable] -> [G.Mat4] -> DrawFun
draw camera unifs meshes xfrms alpha = sequence_ $ zipWith3 draw1 unifs meshes xfrms
    where draw1 unif mesh xfrm = do
              let unifset shdr' alpha' = do unif shdr alpha
                                            G.setUniform shdr' "modelView" (camera !*! xfrm)
              drawObject mesh unifset alpha
              where shdr = objShader mesh

main :: IO ()
main = withWindow help
    where help wnd = do
              GLFW.setKeyCallback wnd (Just keyCB)
              shdr <- G.simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
              obj <- loadWavefront shdr "assets/capsule.obj"
              tex <- loadTex "assets/capsule.png"
              return $ do
                  (fbWidth, fbHeight) <- GLFW.getFramebufferSize wnd
                  let camera = G.perspective 0.1 100 (pi/2) (fromIntegral fbWidth / fromIntegral fbHeight)
                      xfrm = mkTransformation (axisAngle (V3 0 1 0) (pi/2))  (V3 0 0 (-3))
                      unif shdr' alpha = do
                          G.activeTexture G.$= G.TextureUnit 0
                          G.textureBinding G.Texture2D G.$= Just tex
                          G.setUniform shdr' "tex" (0 :: G.GLint)
                  return (draw camera [unif] [obj] [xfrm])
