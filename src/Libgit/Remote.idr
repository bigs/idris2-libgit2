module Libgit.Remote

import Control.Monad.Managed

import Libgit.FFI
import Libgit.Types

withRemote : GitRepository -> String -> (GitResult GitRemote -> IO a) -> IO a
withRemote (MkGitRepository repoPtr) name act = do
  let cgr = git_lookup_remote repoPtr name
  (err, ptr) <- getGitResultPair cgr
  res <- act (MkGitRemote <$> toGitResult err ptr)
  primIO (prim_git_remote_free ptr)
  pure res

||| Lookup a repository remote by its name.
|||
||| Returns on success a managed reference to a Git remote
||| Returns on failure a Git error code.
|||
||| @repo The Git repository.
||| @name The string name of the remote.
export
remote : (repo : GitRepository)
      -> (name : String)
      -> Managed (GitResult GitRemote)
remote repo name = managed (withRemote repo name)

||| Download new data and update tips from a Git remote.
|||
||| Returns a Git error code.
|||
||| @remote The Git remote to fetch.
remoteFetch : (remote : GitRemote) -> IO Int
remoteFetch (MkGitRemote remote) = do
  let cgr = git_fetch_options_init
  (0, ptr) <- getGitResultPair cgr
    | (err, ptr) => pure err <* primIO (prim_free ptr)
  res <- primIO (prim_git_remote_fetch remote null_ptr ptr null_string)
  pure res <* primIO (prim_free ptr)

||| Download new data and update tips from a Git remote.
|||
||| Returns a Git error code.
|||
||| @remote The Git remote to fetch.
||| @reflogMessage The message to write in the reflog for this fetch.
remoteFetch' : (remote : GitRemote)
            -> (reflogMessage : String)
            -> IO Int
remoteFetch' (MkGitRemote remote) reflogMessage = do
  let cgr = git_fetch_options_init
  (0, ptr) <- getGitResultPair cgr
    | (err, ptr) => pure err <* primIO (prim_free ptr)
  res <- primIO (prim_git_remote_fetch remote null_ptr ptr (make_string reflogMessage))
  pure res <* primIO (prim_free ptr)
