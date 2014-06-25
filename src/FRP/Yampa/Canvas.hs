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
	(Blank.Event -> Canvas (Event a))
     -> (b -> Canvas ())
     -> SF (Event a) b
     -> DeviceContext -> IO ()
reactimateSFinContext interpEvent putCanvasOutput sf context =
  do clock <- newClock

     let getInput :: Bool -> IO (DTime,Maybe (Event a))
         getInput canBlock =
            do let opt_block m =
                            if canBlock
                            then m
                            else m `orElse` return Nothing

               opt_e <- atomically $ opt_block $ fmap Just $ readTChan (eventQueue context)
               ev <- case opt_e of
                       Nothing -> return NoEvent
                       Just e  -> send context (interpEvent e)

               t <- clockTick clock
               return (t, Just ev)

         putOutput :: Bool -> b -> IO Bool
         putOutput changed b = if changed
                                 then renderCanvas context (putCanvasOutput b) >> return False
                                 else return False

     reactimate (return NoEvent) getInput putOutput sf

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
