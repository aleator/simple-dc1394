{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import CV.ColourUtils
import CV.Conversions
import CV.Image
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Array.CArray
import Data.Array.IArray
import Data.Bits
import Foreign.C.Types
import Control.Concurrent (threadDelay)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as B

import Data.Enumerator hiding (peek)
import qualified Data.Enumerator.List as E
import Control.Monad.Trans

import System.Camera.Firewire.Simple


enumCamera
  :: MonadIO m =>
     Camera -> Step (Image RGB D8) m b -> Iteratee (Image RGB D8) m b
enumCamera camera = loop 
    where
     loop (Continue k) = do
                            x <- liftIO $ getFrame camera
                            case x of
                                Just i -> k (Chunks [i]) >>== loop  
                                Nothing -> k (Chunks []) >>== loop 
     loop s = returnI s

save :: Iteratee (Image RGB D8) IO ()
save = continue $ go 0
 where
    go :: Int -> Stream (Image RGB D8) -> Iteratee (Image RGB D8) IO ()
    go n (Chunks []) = liftIO (print "No-SNAP") >> continue (go (n))
    go n (Chunks [i]) = liftIO (print "SNAP") >> liftIO (saveImage ("img_"++show n++".png") i) >> continue (go (n+1))
    go n a = yield () a 


saveClip c = enumCamera c $$ (E.isolate 10 =$ save)

main = withDC1394 $ \dc -> do
    (e:_) <- getCameras dc
    print e
    print ("Trying camera", e)
    cam <- cameraFromID dc e-- c'dc1394_camera_new dc guid
    print ("Camera can do oneshots", oneShotCapable cam)
    setISOSpeed  cam ISO_800
    setVideoMode cam Mode_640x480_RGB8
    setFrameRate cam Rate_7_5
    setupCamera cam 4 (defaultFlags)
    startVideoTransmission cam
    
   --  run_ (saveClip cam)
    getFrame cam >>= maybe (return ()) (saveImage "testShot2-1.png")
    getFrame cam >>= maybe (return ()) (saveImage "testShot2-3.png")
    threadDelay (1000000)
    flushBuffer cam
    startVideoTransmission cam
    getFrame cam >>= maybe (return ()) (saveImage "testShot2-4.png")
    
    stopVideoTransmission cam
    stopCapture cam


