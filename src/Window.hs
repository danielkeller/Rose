module Window (
    withWindow,
    Action, DrawFun,
) where

import GHC.IO.Encoding (setForeignEncoding, utf8)
import Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad
import Data.Time
import Graphics
import Types

resizeCB :: WindowSizeCallback
resizeCB _ w h = viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

type Action = Window -> IO (IO DrawFun)

withWindow :: Action -> IO ()
withWindow action = 
    do res <- GLFW.init
       setErrorCallback (Just (const error))
       windowHint (WindowHint'ContextVersionMajor 3)
       windowHint (WindowHint'ContextVersionMinor 3)
       windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
       setForeignEncoding utf8 --fix utf-8 chars in window title
       when res (run =<< createWindow 1024 768 "λ 3D" Nothing Nothing)
    `finally` terminate
    where run Nothing = return ()
          run (Just wnd) = do
              makeContextCurrent (Just wnd)
              setWindowSizeCallback wnd (Just resizeCB)
              cullFace $= Just Back
              depthFunc $= Just Less
              back <- getCurrentTime
              tick <- action wnd
              mainLoop wnd back (addUTCTime dt back) (const (return ())) tick
            `finally` destroyWindow wnd

mainLoop :: Window -> UTCTime -> UTCTime -> DrawFun -> IO DrawFun -> IO ()
mainLoop wnd back -- completed time
             front -- end of current tick
             drawfn -- current render action
             tick -- current physics state
  = do
    now <- getCurrentTime
    let back' = min (addUTCTime 0.25 back) now
    printError --pausing with GDebugger here breaks the main loop
    close <- windowShouldClose wnd
    
    unless close $ if back' >= front
        --we have enough unsimulated time to need a new physics step
        then do
            pollEvents
            drawfn' <- tick 
            let front' = addUTCTime dt back'
            --putStrLn $ "Sim " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front')
            mainLoop wnd back' front' drawfn' tick
        --the current draw action is still fresh
        else do
            clear [ColorBuffer, DepthBuffer]
            --the alpha is the amount of the physics step we have left to display
            let alpha = 1 - (realToFrac $ front `diffUTCTime` back) / dt
            drawfn alpha
            swapBuffers wnd
            --putStrLn $ "Draw " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front)
            mainLoop wnd back' front drawfn tick

dt :: Fractional a => a
dt = 0.03