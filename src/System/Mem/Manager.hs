{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness -fno-float-in #-}
module System.Mem.Manager
  ( currentResidentSetSize
  , peakResidentSetSize
  , hardPageFaults
  , checkMemoryPressure
  , selfishManager
  , cumulativeManagedCollections
  ) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign.C.Types
import System.IO.Unsafe
import System.Mem

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

managedCollections :: IORef Int
managedCollections = unsafePerformIO $ newIORef 0
{-# NOINLINE managedCollections #-}

-- | Run a background thread that checks for memory pressure from the Host OS and kickstarts a GC.
selfishManager :: IO ThreadId
selfishManager = forkIO $ forever $ do
  b <- checkMemoryPressure
  when b $ do
    performMajorGC
    modifyIORef' managedCollections (+1)
  threadDelay 50000

-- | Return the total number of managed collections that the GC manager has forced.
cumulativeManagedCollections :: IO Int
cumulativeManagedCollections = readIORef managedCollections
{-# INLINE cumulativeManagedCollections #-}
