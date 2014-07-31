{-# LANGUAGE TypeOperators, DataKinds, TemplateHaskell #-}
module Universe (
    Everything, nothing,

    Uniforms, uniforms,
    Renders, renders,
    Xforms, xforms, oldXforms,
    scene, camera,
) where

import Control.Lens
import Linear (eye4)

import Types
import Render
import qualified Graphics as G
import qualified Data.Map as M
import Object

type Uniforms = M.Map Object UnifSetter
type Renders = M.Map Object Renderable
type Xforms = M.Map Object G.Xform

data Everything = Everything { _uniforms :: Uniforms
                             , _renders :: Renders
                             , _xforms :: Xforms
                             , _oldXforms :: Xforms
                             , _scene :: Scene
                             , _camera :: G.Mat4
                             --, _script :: Env
                             }

--how poetic.
nothing :: Everything
nothing = Everything M.empty M.empty M.empty M.empty emptyScene eye4

makeLenses ''Everything