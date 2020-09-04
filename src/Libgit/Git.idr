module Libgit.Git

import Control.Monad.Reader
import Control.Monad.Managed
import Control.Monad.State
import Control.Monad.Trans
import System.FFI

import Libgit.FFI
import Libgit.Types

||| Runs some action within an initialized Git context. Initializes libgit2
||| static memory before running the action and shuts it down after running the
||| action. All managed Git resources must be accessed from within this context.
export
withGit : HasIO io => io b -> io (Either Int b)
withGit act = do
  err <- liftIO (primIO prim_libgit_init)
  case err < 0 of
    True => pure (Left err)
    False => do
      res <- act
      liftIO (primIO prim_libgit_shutdown)
      pure (Right res)
