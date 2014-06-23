{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module FRP.Yampa.Canvas (reactimateSFinContext) where

import FRP.Yampa

import Data.Time.Clock
import Data.IORef
import Control.Concurrent.STM

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank

-------------------------------------------------------------------

-- | Redraw the entire canvas.
renderCanvas ::  DeviceContext -> Canvas () -> IO ()
renderCanvas context drawAction = send context canvas
  where
    canvas :: Canvas ()
    canvas = do clearCanvas
                beginPath ()
                saveRestore drawAction

-------------------------------------------------------------------

type Clock = IORef UTCTime

-- | A specialisation of 'FRP.Yampa.reactimate' to Blank Canvas.
--   The arguments are: the Canvas action to get input, the Canvas action to emit output, the signal function to be run, and the device context to use.
reactimateSFinContext 
      :: forall a b. 
	((Float,Float) -> Blank.Event -> a) 
     -> (b -> Canvas ()) 
     -> SF a b 
     -> DeviceContext -> IO ()
reactimateSFinContext interpInput putCanvasOutput sf context =
  do clock <- newClock

     let size = (width context, height context)

         getInput0 :: IO a
         getInput0 = return 
	 	   $ interpInput size
		   $ Blank.Event { eMetaKey = False
		     		 , ePageXY = Nothing
				 , eType = "init"
				 , eWhich = Nothing }

         getInput :: Bool -> IO (DTime,Maybe a)
         getInput canBlock =
            do let opt_block m = 
                            if canBlock 
                            then m
                            else m `orElse` return Nothing
               a <- atomically $ opt_block $ do
                    e <- readTChan (eventQueue context)
                    return (Just e) 
               t <- clockTick clock
	       print (a,t)
               return (t,fmap (interpInput size) a)

         putOutput :: Bool -> b -> IO Bool
         putOutput changed b = if changed
                                 then renderCanvas context (putCanvasOutput b) >> return False
                                 else return False

     reactimate getInput0 getInput putOutput sf

-- | Start a new clock.
newClock :: IO Clock
newClock = getCurrentTime >>= newIORef

-- | Compute the time delta since the last clock tick.
clockTick :: Clock -> IO DTime
clockTick x =
    do t0 <- readIORef x
       t1 <- getCurrentTime
       writeIORef x t1
       return (realToFrac (diffUTCTime t1 t0))

-------------------------------------------------------------------
