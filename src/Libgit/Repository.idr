module Libgit.Repository

import Libgit.FFI
import Libgit.Git
import Libgit.Types

||| Open a local git repository. Libgit2 will determine whether or not the
||| repository is bare.
|||
||| Returns on success a `GitRepository` indexed by the current Git session.
||| Returns on failure an `Int` Git error code.
|||
||| @path The path to the local Git repository.
export
openGitRepository : (Applicative m, HasIO m)
                 => (path : String)
                 -> GitT i m (GitResult (GitRepository i))
openGitRepository path = do
  repoPtr <- liftPIO prim_mk_null_git_repository
  0 <- liftPIO $ prim_git_repository_open repoPtr path
    | res => gitError res
  let repo = MkGitRepository (get_git_repository repoPtr)
  gitSuccess repo

||| Executes an action with an opened GitRepository.
|||
||| Returns on success the result of `action` with the GitRepository.
||| Returns on failure the `Int` Git error code.
|||
||| @path   The path to the local Git repository.
||| @action The action to run with the GitRepository
withGitRepository : (HasIO m, Monad m)
                 => (path : String)
                 -> (action : GitRepository i -> GitT i m (GitResult a))
                 -> GitT i m (GitResult a)
withGitRepository path action = do
  result <- openGitRepository path
  join <$> traverse action result
