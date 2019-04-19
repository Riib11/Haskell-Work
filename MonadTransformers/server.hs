{-# LANGUAGE PolyKinds #-}

import Control.Monad
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans.Reader
import Control.Concurrent

-- each client thread is of type IO ()
-- since forkIO :: IO () -> IO ThreadId

data Config

newtype MyReaderT r (m :: k -> *) (a :: k) = ReaderT {runMyReaderT :: r -> m a}

-- generic client function
client_func :: ReaderT Config IO ()
client_func = error "unimplemented"
