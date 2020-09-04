module Libgit.Git

import Control.Monad.Reader
import Control.Monad.Managed
import Control.Monad.State
import Control.Monad.Trans
import System.FFI

import Libgit.FFI
import Libgit.Types

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
