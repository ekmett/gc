{-# LANGUAGE ForeignFunctionInterface #-}
module System.RSS 
  ( currentRSS
  , peakRSS
  ) where

import Foreign.C.Types

-- | Retrieve the current resident set size for the currently executing program.
foreign import ccall "getCurrentRSS" currentRSS :: IO CSize

-- | Retrieve the current peak resident set size for the currently executing program.
foreign import ccall "getPeakRSS" peakRSS :: IO CSize
