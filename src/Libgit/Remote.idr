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
