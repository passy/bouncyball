{-# LANGUAGE Arrows #-}

module Main where

import Data.IORef
import FRP.Yampa
import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)

import Graphics (initGL, draw)
import Types

fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall y0 v0 =
    -- First time I'm using the Arrow syntax. Feels weird.
    proc _ -> do
        v <- integral >>^ (+ v0) -< (-9.81)
        y <- integral >>^ (+ y0) -< v
        returnA -< (y, v)

bouncingBall :: Pos -> Vel -> SF () (Pos, Vel)
bouncingBall y0 v0 = switch (bb y0 v0) bswitch
    where
        bb y0' v0' = proc input -> do
            (pos, vel) <- fallingBall y0' v0' -< input
            -- If the condition input is true, we fire an event which is
            -- caught by the switch above.
            event' <- edge -< pos <= 0
            returnA -< ((pos, vel), event' `tag` (pos, vel))
        bswitch (pos, vel)
            | abs vel < 1.0 = constant (0.0, 0.0)
            | otherwise     = bouncingBall pos (-vel * 0.6)

mainSF :: SF () (IO ())
mainSF = (bouncingBall 10.0 0.0) >>^ \ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> draw pos

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
