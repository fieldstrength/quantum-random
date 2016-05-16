-- | Used to coordinate access to the store and settings files to ensure that concurrent
--   processes cannot interfere by operating on the files at the same time. Uses an `MVar`.
--
--   It could also be used to control access to /any/ resource, not just these particular files.
--
--   Usually to be imported via the "Quantum.Random" module.
module Quantum.Random.Semaphore where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)

-- | A data type holding an @MVar ()@ – in short, a container in which a unit type may or may
--   not be present – in order to coordinate access to the local files. When the unit is present,
--   access is available. IO operations that need access to the store or settings file remove it
--   from the `MVar` before doing so, and then replace it when they're done. Then whenever two such
--   operations might otherwise interfere, they will instead wait their turn to obtain the access.
data AccessControl = AccessControl (MVar ())

-- | Initiate the access control system.
initAccessControl :: IO AccessControl
initAccessControl = AccessControl <$> newMVar ()

-- | Perform the supplied IO action only when access is granted.
withAccess :: AccessControl -> IO a -> IO a
withAccess (AccessControl mv) io = do
  _ <- takeMVar mv
  x <- io
  putMVar mv ()
  pure x
