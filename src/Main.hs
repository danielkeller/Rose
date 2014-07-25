module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Data.List(zipWith4)
import Control.Lens.Lens
import Control.Lens.Setter

import Window
import Wavefront
import qualified Graphics as G
import Types
import Render
import Texture
import Math.BVH
import Collision

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()

draw :: G.Mat4 -> [UnifSetter] -> [Renderable] -> [G.Xform] -> [G.Xform] -> DrawFun
draw camera unifs meshes xfrms oldXfrms alpha = sequence_ $ zipWith4 draw1 unifs meshes xfrms oldXfrms
    where draw1 unif mesh xfrm oldXfrm = do
              let unifset shdr' _ = do unif shdr alpha
                                       let xfrmMat = G.toMat4 (G.slide alpha oldXfrm xfrm)
                                       G.setUniform shdr' "modelView" (camera !*! xfrmMat)
              drawObject mesh unifset alpha
              where shdr = objShader mesh

noUnifs :: UnifSetter
noUnifs _ _ = return ()

main :: IO ()
main = withWindow $ \wnd -> do
    GLFW.setKeyCallback wnd (Just keyCB)
    shdr <- G.simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
    (obj, mesh) <- loadWavefront shdr "assets/capsule.obj"
    bvhObj <- drawAABB (buildBVH mesh)
    tex <- loadTex "assets/capsule.png"
    let xfrm1 = G.Xform (V3 0 0 (-3)) (axisAngle (V3 0 1 0) (pi/2)) 1
    let tick xfrm = do
        (fbWidth, fbHeight) <- GLFW.getFramebufferSize wnd
        let xfrm' = xfrm & G.rotation %~ (* axisAngle (V3 0 1 0) dt)
        let camera = G.perspective 0.1 100 (pi/2) (fromIntegral fbWidth / fromIntegral fbHeight)
            unif shdr' _ = do
                G.activeTexture G.$= G.TextureUnit 0
                G.textureBinding G.Texture2D G.$= Just tex
                G.setUniform shdr' "tex" (0 :: G.GLint)
        return (draw camera [unif, noUnifs] [obj, bvhObj] [xfrm', xfrm'] [xfrm, xfrm], xfrm')
    return (tick, xfrm1)
