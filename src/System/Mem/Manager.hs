{-# LANGUAGE ForeignFunctionInterface #-}
module System.Mem.Manager
  ( currentResidentSetSize
  , peakResidentSetSize
  , hardPageFaults
  , checkMemoryPressure
  ) where

import Data.IORef
import Foreign.C.Types
import System.IO.Unsafe

-- | Retrieve the current resident set size for the currently executing program.
foreign import ccall "getCurrentRSS" currentResidentSetSize :: IO CSize

-- | Retrieve the current peak resident set size for the currently executing program.
foreign import ccall "getPeakRSS" peakResidentSetSize :: IO CSize

foreign import ccall "getHardPageFaults" hardPageFaults :: IO CSize

data ManagerState
  = NoManagerState
  | ManagerState { _faults, _rss  :: {-# UNPACK #-} !CSize }

managerState :: IORef ManagerState
managerState = unsafePerformIO $ newIORef NoManagerState
{-# NOINLINE managerState #-}

-- | Determine if memory pressure warrants further action.
checkMemoryPressure :: IO Bool
checkMemoryPressure = do
  old <- readIORef managerState 
  newRss <- currentResidentSetSize
  newFaults <- hardPageFaults
  writeIORef managerState (ManagerState newRss newFaults)
  return $! case old of
    NoManagerState -> False
    ManagerState oldRss oldFaults -> newRss < oldRss || newFaults >= oldFaults + 10
