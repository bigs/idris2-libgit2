module Libgit.Git

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

||| Get the last Git error, if present.
|||
||| Returns a tuple of a git error message and an error class code if a Git
||| error is present.
export
lastError : Maybe (String, Int)
lastError =
  let ptr = git_error_last in
    case is_null_ptr (prim__forgetPtr ptr) of
      1 => Nothing
      _ => let giterr = derefGitError ptr
               message = getField giterr "message"
               klass = getField giterr "klass"
               messageStr = getString message in
             Just (messageStr, klass)
