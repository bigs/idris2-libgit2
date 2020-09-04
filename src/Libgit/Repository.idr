module Libgit.Repository

import Control.Monad.Managed

import Libgit.Clone
import Libgit.FFI
import Libgit.Git
import Libgit.Types

public export
data GitRepositoryOptions : Type where
  Clone : CloneOpts -> (url : String) -> (localPath : String) -> GitRepositoryOptions
  Open : (path : String) -> GitRepositoryOptions

withOpenedRepository : (path : String)
                    -> (action : (GitResult GitRepository -> IO a)
                    -> IO a
withOpenedRepository path act = do
  cresult <- primIO (prim_git_open_repository path)
  result <- getGitResult cresult
  res <- act (MkGitRepository <$> result)
  case result of
    Left _ => pure res
    Right ptr => pure res <* primIO (prim_git_repository_free ptr)

managedOpenedRepository : (path : String)
                       -> Managed (GitResult GitRepository)
managedOpenedRepository path = managed (withOpenedRepository path)

export
managedRepository : GitRepositoryOptions
                 -> Managed (GitResult GitRepository)
managedRepository (Clone opts url localPath) = managedClonedRepository opts url localPath
managedRepository (Open path) = managedOpenedRepository path
