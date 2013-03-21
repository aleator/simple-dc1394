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
    cam <- cameraFromID dc e
    setOperationMode cam B
    setISOSpeed  cam ISO_800
    setFrameRate cam Rate_15
    setupCamera cam 4 (defaultFlags)
    
    print "Starting to Acquire"
    withVideoMode cam $ \(c :: Camera Mode_1280x960_RGB8) -> do
        startVideoTransmission c
        getFrame c >>= maybe (return ()) (saveImage "testShot2-1.png")
        getFrame c >>= maybe (return ()) (saveImage "testShot2-3.png")
        threadDelay (1000000)
        flushBuffer c
        startVideoTransmission c
        forkIO $ replicateM_ 50 (print "frame" >> getFrame c >>= maybe (return ()) (saveImage "testShot2-4.png"))
         
        threadDelay (100000000)
        stopVideoTransmission c
        stopCapture c


