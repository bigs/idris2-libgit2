module Libgit.Repository

import Control.Monad.Managed

import Libgit.Clone
import Libgit.FFI
import Libgit.Git
import Libgit.Types

||| A sum type representing the different ways to instantiate a GitRepository.
public export
data GitRepositoryOptions : Type where
  ||| Clone a repository given options, a URL, and a local path.
  Clone : CloneOpts -> (url : String) -> (localPath : String) -> GitRepositoryOptions
  ||| Open an existing repository using a local path.
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

||| Opens an existing Git repository.
|||
||| Returns the Git repository pointed to by the provided path.
|||
||| @path Local path to the Git repository.
export
openedRepository : (path : String)
                -> Managed (GitResult GitRepository)
openedRepository path = managed (withOpenedRepository path)

||| Given GitRepositoryOptions, obtain a managed reference to a GitRepository
||| by cloning a repository or opening an existing repository.
|||
||| Returns a managed reference to a GitRepository.
|||
||| @options A GitRepositoryOptions specifying the strategy to use.
export
repository : (options : GitRepositoryOptions)
          -> Managed (GitResult GitRepository)
repository (Clone opts url localPath) = clonedRepository opts url localPath
repository (Open path) = openedRepository path
