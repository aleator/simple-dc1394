{-#LANGUAGE ScopedTypeVariables, DataKinds #-}
module Main where

import CV.ColourUtils
import CV.Conversions
import CV.Image
import Control.Applicative
import Control.Monad
import Control.Concurrent

import System.Camera.Firewire.Simple


main = withDC1394 $ \dc -> do
    (e:_) <- getCameras dc
    print e
    print ("Trying camera", e)
    cam <- cameraFromID dc e-- c'dc1394_camera_new dc guid
    print ("Camera can do oneshots", oneShotCapable cam)
    setISOSpeed  cam ISO_200
    setFrameRate cam Rate_3_75
    setupCamera cam 1 (defaultFlags &+ autoISO)
    
   --  run_ (saveClip cam)
    withVideoMode cam $ \ (c :: Camera Mode_1280x960_RGB8) -> do
        startVideoTransmission c
        getFrame c >>= maybe (return ()) (saveImage "testShot2-1.png")
        getFrame c >>= maybe (return ()) (saveImage "testShot2-3.png")
        threadDelay (1000000)
        flushBuffer c
        startVideoTransmission c
        getFrame c >>= maybe (return ()) (saveImage "testShot2-4.png")
        
        stopVideoTransmission c
        stopCapture c


