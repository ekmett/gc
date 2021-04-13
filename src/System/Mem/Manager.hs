{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright   :  (C) 2015-2021 Edward Kmett
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
-- Due to the fact that Haskell returns memory to the operating system and doesn't really
-- tell me about it, this follows their 'GenMS+Selfish' (without RSS) scheme.
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


module System.Mem.Manager
  ( -- * Memory Manager
    selfishManager
    -- * Statistics and Utilities
  , hardPageFaults
  ) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign.C.Types
import System.Mem

-- | Return the total number of \"hard page-faults\" since the program started. These are page-faults which required us to go out to disk.
foreign import ccall "getHardPageFaults" hardPageFaults :: IO CSize

-- | Run a background thread that checks for signs of memory pressure from the Host OS and kickstarts a garbage collection as needed. Returns the thread for the selfish gc manager and an IO action
-- that can be run to count the cumulative number of managed collections
selfishManager :: IO (ThreadId, IO Int)
selfishManager = do
  collections <- newIORef 0
  threadId <- forkIO $ do
    faults <- hardPageFaults
    let go oldFaults = do
          newFaults <- hardPageFaults
          when (newFaults >= oldFaults + 10) $ do
            performMajorGC
            modifyIORef' collections (+1)
          threadDelay 50000
          go newFaults
    go faults
  pure (threadId, readIORef collections)
