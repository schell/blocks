{-# LANGUAGE CPP #-}
module App.Clock where

#ifdef darwin_HOST_OS

import           Control.Applicative ( (<$>) )
import           Data.Time.Clock.POSIX

getTime :: IO Double
getTime = realToFrac <$> getPOSIXTime

#else

import           Control.Applicative ( (<$>) )
import qualified System.Clock as SysClk

getTime :: IO Double
getTime =
    toSeconds <$> SysClk.getTime SysClk.Monotonic
  where
    toSeconds (SysClk.TimeSpec seconds nanos) =
        fromIntegral seconds + fromIntegral nanos * 10**(-9)

#endif

data Clock = Clock { _frames   :: [Double]
                   , _avgFPS   :: Double
                   , _timeNow  :: Double
                   , _timePrev :: Double
                   }

emptyClock :: Clock
emptyClock = Clock (replicate 100 0) 0.0 0.0 0.0

tickClock :: Double -> Clock -> Clock
tickClock t clk =
    let fs   = _frames clk
        prev = _timeNow clk
        len  = length fs
        dt   = t - prev
    in Clock { _timePrev = prev
             , _timeNow  = t
             , _frames   = take len (dt:fs)
             , _avgFPS   = 1/(sum fs /fromIntegral len)
             }


