{-# LANGUAGE ForeignFunctionInterface #-}
module System.Mem.RSS 
  ( currentResidentSetSize
  , peakResidentSetSize
  ) where

import Foreign.C.Types

-- | Retrieve the current resident set size for the currently executing program.
foreign import ccall "getCurrentRSS" currentResidentSetSize :: IO CSize

-- | Retrieve the current peak resident set size for the currently executing program.
foreign import ccall "getPeakRSS" peakResidentSetSize :: IO CSize
