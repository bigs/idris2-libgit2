module Libgit.Clone

import Prelude
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Syntax
import Control.Monad.Trans
import System.FFI

import Libgit.FFI
import Libgit.Git
import Libgit.Types

||| A set of options that dictate how a repository should be cloned from a
||| remote.
public export
record CloneOpts where
  constructor MkCloneOpts
  ||| Make a bare Git repository. When enabled, the localPath provided to clone
  ||| will contain the administrative files usually contained within .git and
  ||| files won't actually be checked out.
  bare : Bool
  ||| Which branch to checkout after cloning.
  checkoutBranch : String

||| Sensible defaults for CloneOpts.
||| + bare = False
||| + checkoutBranch = master
export
defaultOpts : CloneOpts
defaultOpts = MkCloneOpts False "master"

applyOpts : CloneOpts -> GCAnyPtr -> IO ()
applyOpts cloneOpts cCloneOpts = do
  let bare' = cBool cloneOpts.bare
  primIO $ prim_apply_clone_options cCloneOpts cloneOpts.checkoutBranch bare'

initGitCloneOptions : HasIO m => CloneOpts -> GitT i m (GitResult GCAnyPtr)
initGitCloneOptions opts = do
  cloneOptionsUnmanaged <- liftPIO prim_init_clone_options
  cloneOptions <- liftIO (managePtr "clone options" cloneOptionsUnmanaged)
  0 <- liftPIO $ prim_git_clone_init_options cloneOptions git_clone_options_version
    | res => pure $ Left res
  liftIO $ applyOpts opts cloneOptions
  pure $ Right cloneOptions

||| Clones a Git repository from a remote.
|||
||| Returns on failure an `Int` representing a Git error code.
||| Returns on success a `GitRepository` indexed by the current Git session.
|||
||| @opts      A CloneOpts specifying how the repository should be cloned.
||| @url       The URL to the Git remote to clone from.
||| @localPath The local path to clone the repository to.
export
clone : (Applicative m, HasIO m)
     => (opts : CloneOpts)
     -> (url : String)
     -> (localPath : String)
     -> GitT i m (Either Int (GitRepository i))
clone opts url localPath = do
  Right options <- initGitCloneOptions {i} opts
    | Left res => pure (Left res)
  cresult <- liftPIO $ prim_git_clone_repository url localPath options
  result <- liftIO (gitResultWithFinalizer prim_git_repository_free "git repository" cresult)
  gResult <- traverse toGit result
  pure (MkGitRepository <$> gResult)
