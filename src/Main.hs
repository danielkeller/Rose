{-# LANGUAGE Rank2Types #-}
module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Control.Lens.Lens
import Control.Lens.At
import Control.Lens.Setter
import Control.Lens.Getter
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Window
import Wavefront
import qualified Graphics as G
import Types
import Render
import Texture
import Math.BVH
import Collision
import Object
import Universe

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()

draw :: Everything -> DrawFun
draw e alpha = do
    mapM_ draw1 (M.keys meshes)
    where meshes = e ^. renders
          draw1 object = do
              let shdr = objShader mesh
                  unifset shdr' _ = do getit uniforms shdr alpha
                                       let xfrmMat = G.toMat4 (G.slide alpha (getit oldXforms) (getit xforms))
                                       G.setUniform shdr' "modelView" (e ^. camera !*! xfrmMat)
              drawObject mesh unifset alpha
              where getit :: Lens' Everything (M.Map Object a) -> a
                    getit l = fromJust $ e ^. l . at object
                    mesh = getit renders

noUnifs :: UnifSetter
noUnifs _ _ = return ()

simpleObject :: UnifSetter -> Renderable -> G.Xform -> Everything -> (Everything, Object)
simpleObject unif mesh xfrm e = (e', obj)
    where (scene', obj) = newObject $ e ^. scene
          e' = e & renders . at obj ?~ mesh & uniforms . at obj ?~ unif & xforms . at obj ?~ xfrm & scene .~ scene'

main :: IO ()
main = withWindow $ \wnd -> do
    GLFW.setKeyCallback wnd (Just keyCB)
    shdr <- G.simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
    (render, mesh) <- loadWavefront shdr "assets/capsule.obj"
    tex <- loadTex "assets/capsule.png"
    let xfrm = G.Xform (V3 0 0 (-3)) (axisAngle (V3 0 1 0) (pi/2)) 1
        unif shdr' _ = do
            G.activeTexture G.$= G.TextureUnit 0
            G.textureBinding G.Texture2D G.$= Just tex
            G.setUniform shdr' "tex" (0 :: G.GLint)
        (e1, obj) = simpleObject unif render xfrm nothing
    bvhRender <- drawAABB (buildBVH mesh)
    let (e2, bvhObj) = simpleObject noUnifs bvhRender xfrm e1
    let tick e = do
        (fbWidth, fbHeight) <- GLFW.getFramebufferSize wnd
        let moveit = G.rotation %~ (* axisAngle (V3 0 1 0) dt)
        let e' = e & oldXforms .~ (e^.xforms) & xforms %~ ((ix obj %~ moveit) . (ix bvhObj %~ moveit))
                  & camera .~ G.perspective 0.1 100 (pi/2) (fromIntegral fbWidth / fromIntegral fbHeight)
        return (draw e', e')
    return (tick, e2)
