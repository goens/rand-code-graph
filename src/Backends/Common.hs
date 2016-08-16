module Backends.Common where


import           Data.IORef
import           System.IO.Unsafe


inlineIfBranches :: IORef Bool
inlineIfBranches = unsafePerformIO $ newIORef False


shouldInlineIfBranches :: a -> Bool
shouldInlineIfBranches _ = unsafePerformIO (readIORef inlineIfBranches)
