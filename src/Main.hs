module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.Trans.Class
import Linear
import Data.List(zipWith4)
import Control.Lens.Lens
import Control.Lens.At
import Control.Lens.Setter
import qualified Data.Map as M

import Window
import Wavefront
import qualified Graphics as G
import Types
import Render
import Texture
import Math.BVH
import Collision
import Object
import Globals

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()

type Uniforms = M.Map Object UnifSetter
gUniforms :: Global Uniforms
gUniforms = Global M.empty "uniforms"

type Renders = M.Map Object Renderable
gRenders :: Global Renders
gRenders = Global M.empty "renders"

type Xforms = M.Map Object G.Xform
gXforms :: Global Xforms
gXforms = Global M.empty "xforms"
gOldXforms :: Global Xforms
gOldXforms = Global M.empty "oldxforms"

gScene :: Global Scene
gScene = Global emptyScene "scene"
gCamera :: Global G.Mat4
gCamera = Global eye4 "camera"

draw :: DrawFun
draw alpha = do
    meshes <- get gRenders
    mapM_ draw1 (M.keys meshes)
    where draw1 object = do
              mesh <- gets gRenders getit
              unif <- gets gUniforms getit
              xfrm <- gets gXforms getit
              oldXfrm <- gets gOldXforms getit
              camera <- get gCamera
              let shdr = objShader mesh
                  unifset shdr' _ = do unifSetter unif shdr alpha
                                       let xfrmMat = G.toMat4 (G.slide alpha oldXfrm xfrm)
                                       G.setUniform shdr' "modelView" (camera !*! xfrmMat)
              drawObject mesh (UnifSetter unifset) alpha
              where getit = (M.! object)

noUnifs :: UnifSetter
noUnifs = UnifSetter $ \ _ _ -> return ()

simpleObject :: UnifSetter -> Renderable -> G.Xform -> Globals Object
simpleObject unif mesh xfrm = do
    (scene', obj) <- gets gScene newObject
    put gScene scene'
    modify gUniforms $ M.insert obj unif
    modify gXforms $ M.insert obj xfrm
    modify gRenders $ M.insert obj mesh
    return obj

main :: IO ()
main = withWindow $ \wnd -> do
    lift $ GLFW.setKeyCallback wnd (Just keyCB)
    shdr <- lift $ G.simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
    (render, mesh) <- lift $ loadWavefront shdr "assets/capsule.obj"
    tex <- lift $ loadTex "assets/capsule.png"
    let xfrm = G.Xform (V3 0 0 (-3)) (axisAngle (V3 0 1 0) (pi/2)) 1
        unif shdr' _ = do
            G.activeTexture G.$= G.TextureUnit 0
            G.textureBinding G.Texture2D G.$= Just tex
            G.setUniform shdr' "tex" (0 :: G.GLint)
    obj <- unwrapGlobalsT $ simpleObject (UnifSetter unif) render xfrm
    bvhRender <- lift $ drawAABB (buildBVH mesh)
    bvhObj <- unwrapGlobalsT $ simpleObject noUnifs bvhRender xfrm
    let tick = do
        (fbWidth, fbHeight) <- lift $ GLFW.getFramebufferSize wnd
        let moveit = G.rotation %~ (* axisAngle (V3 0 1 0) dt)
        xfrms <- get gXforms
        put gXforms $ ix obj %~ moveit $ ix bvhObj %~ moveit $ xfrms
        put gOldXforms xfrms
        put gCamera $ G.perspective 0.1 100 (pi/2) (fromIntegral fbWidth / fromIntegral fbHeight)
        return draw
    return tick
