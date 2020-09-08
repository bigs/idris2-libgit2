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

||| A sum type representing the various strategies for git reset.
|||
||| + GitResetSoft - Move the head to the given commit
||| + GitResetMixed - Soft plus reset index to the commit
||| + GitResetHard - Mixed plus changes in working tree discarded
public export
data GitResetType =
    GitResetSoft
  | GitResetMixed
  | GitResetHard

gitResetTypeToInt : GitResetType -> Int
gitResetTypeToInt GitResetSoft = 1
gitResetTypeToInt GitResetMixed = 2
gitResetTypeToInt GitResetHard = 3

||| Set the current head to a commit or tag.
|||
||| Returns a Git error code.
|||
||| @repo The Git repository containing the commit or tag.
||| @obj  The Git object (must be either a commit or a tag) to set HEAD to.
||| @rst  The type of reset to perform. An instance of GitResetType.
export
resetRepository : (repo : GitRepository)
               -> {typ : GitObjectType}
               -> {auto 0 prf : IsCommitish typ}
               -> (obj : GitObject typ)
               -> (rst : GitResetType)
               -> IO Int
resetRepository (MkGitRepository repoPtr) (MkGitObject objPtr) resetType = do
  let rt = gitResetTypeToInt resetType
      cgrOptions = git_checkout_init_options
  (res, optsPtr) <- getGitResultPair cgrOptions
  let freeOpts = primIO (prim_free optsPtr)
  case res of
    0 => do
      res <- primIO (prim_git_reset repoPtr objPtr rt optsPtr)
      pure res <* freeOpts
    _ => pure res <* freeOpts
