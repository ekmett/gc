module System.RSS 
  ( currentRSS
  , peakRSS
  ) where

foreign import "getCurrentRSS" currentRSS :: IO CSize
foreign import "getPeakRSS" peakRSS :: IO CSize
