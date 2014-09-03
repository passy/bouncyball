module Main where

import FRP.Yampa
import Control.Concurrent

import Graphics (initGL, draw)
import Types

fallingBall :: Pos -> SF () (Pos, Vel)
fallingBall y0 = (constant (-9.81) >>> integral) >>> ((integral >>^ (+ y0)) &&& identity)

main :: IO ()
main =
    reactimate (initGL)
               (\ _ -> threadDelay 100000 >> return (0.1, Nothing))
               (\ _ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> draw pos >> return False)
               (fallingBall 10.0)
