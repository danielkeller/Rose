module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Data.List(zipWith4)

import Window
import Wavefront
import qualified Graphics as G
import Types
import Render
import Texture

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()

draw :: G.Mat4 -> [UnifSetter] -> [Renderable] -> [G.Xform] -> [G.Xform] -> DrawFun
draw camera unifs meshes xfrms oldXfrms alpha = sequence_ $ zipWith4 draw1 unifs meshes xfrms oldXfrms
    where draw1 unif mesh xfrm oldXfrm = do
              let unifset shdr' alpha' = do unif shdr alpha
                                            let xfrmMat = G.toMat4 (G.slide alpha oldXfrm xfrm)
                                            G.setUniform shdr' "modelView" (camera !*! xfrmMat)
              drawObject mesh unifset alpha
              where shdr = objShader mesh

main :: IO ()
main = withWindow $ \wnd -> do
    GLFW.setKeyCallback wnd (Just keyCB)
    shdr <- G.simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
    (obj, _) <- loadWavefront shdr "assets/capsule.obj"
    tex <- loadTex "assets/capsule.png"
    let xfrm = G.Xform (V3 0 0 (-3)) (axisAngle (V3 0 1 0) (pi/2)) 1
    let tick xfrm' = do
        (fbWidth, fbHeight) <- GLFW.getFramebufferSize wnd
        let camera = G.perspective 0.1 100 (pi/2) (fromIntegral fbWidth / fromIntegral fbHeight)
            unif shdr' alpha = do
                G.activeTexture G.$= G.TextureUnit 0
                G.textureBinding G.Texture2D G.$= Just tex
                G.setUniform shdr' "tex" (0 :: G.GLint)
        return (draw camera [unif] [obj] [xfrm'] [xfrm'], xfrm')
    return (tick, xfrm)
