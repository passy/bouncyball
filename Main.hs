{-# LANGUAGE Arrows #-}

module Main where

import Data.IORef
import FRP.Yampa
import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)

import Graphics (initGL, draw)
import Types

fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall y0 v0 =
    proc _ -> do
        v <- integral >>^ (+ v0) -< (-9.81)
        y <- integral >>^ (+ y0) -< v
        returnA -< (y, v)

mainSF :: SF () (IO ())
mainSF = (fallingBall 10.0 0.0) >>^ \ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> draw pos

-- | Main, initializes Yampa and sets up reactimation loop
main :: IO ()
main = do
    oldTime <- newIORef (0 :: Int)
    rh <- reactInit (initGL) (\_ _ b -> b >> return False)
                    mainSF
    displayCallback $= return ()
    idleCallback $= Just (idle oldTime rh)
    oldTime' <- get elapsedTime
    writeIORef oldTime oldTime'
    mainLoop

-- | Reactimation iteration, supplying the input
idle :: IORef Int ->
        ReactHandle () (IO ()) -> IO ()
idle oldTime rh = do
    newTime' <- get elapsedTime
    oldTime' <- get oldTime
    let dt = (fromIntegral $ newTime' - oldTime') / 1000
    _ <- react rh (dt, Nothing)
    writeIORef oldTime newTime'
    return ()
