{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness -fno-float-in #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- This module supplies a variant of the selfish form of
-- <http://www.cs.canisius.edu/~hertzm/prmm-ismm-2011.pdf "Poor Richard's Memory Manager">
-- by Hertz, Kane, Keudel, Bai, Ding, Gu and Bard, adapted to run in
-- Haskell in user-space.
--
-- Usage:
--
-- @
-- main = do
--   _ <- 'selfishManager'
--   ...
-- @
--
-- Now, the background thread that was spawned by 'selfishManager' will watch for signs that the host operating system
-- is starting to cause the current process to page out to disk and respond with more aggressive garbage collection.
--
-- This empowers your code to try to avoid the inevitable death spiral that follows when GC has to happen with paged out data.
-----------------------------------------------------------------------------


module System.Mem.Manager
  ( -- * Memory Manager
    selfishManager
    -- * Statistics and Utilities
  , currentResidentSetSize
  , peakResidentSetSize
  , hardPageFaults
  , checkMemoryPressure
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

-- | Return the total number of \"hard page-faults\" since the program started. These are page-faults which required us to go out to disk.
foreign import ccall "getHardPageFaults" hardPageFaults :: IO CSize

data ManagerState
  = NoManagerState
  | ManagerState { _faults, _rss  :: {-# UNPACK #-} !CSize }

managerState :: IORef ManagerState
managerState = unsafePerformIO $ newIORef NoManagerState
{-# NOINLINE managerState #-}

-- | Determine if memory pressure warrants further action.
--
-- This will return 'True' if we are getting signs from the operating system that we should reign in our memory usage.
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

-- | Run a background thread that checks for signs of memory pressure from the Host OS and kickstarts a garbage collection as needed.
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
