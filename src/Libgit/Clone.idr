module Libgit.Clone

import Prelude
import Control.Monad.Managed
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

applyOpts : CloneOpts -> AnyPtr -> IO ()
applyOpts cloneOpts cCloneOpts = do
  let bare' = cBool cloneOpts.bare
  primIO $ prim_apply_clone_options cCloneOpts cloneOpts.checkoutBranch bare'

withCloneOptions : CloneOpts -> (GitResult AnyPtr -> IO a) -> IO a
withCloneOptions opts act = do
  optsPtr <- primIO prim_init_clone_options
  err <- primIO (prim_git_clone_init_options optsPtr git_clone_options_version)
  applyOpts opts optsPtr
  res <- act (toGitResult err optsPtr)
  primIO (prim_free optsPtr)
  pure res

managedCloneOptions : CloneOpts -> Managed (GitResult AnyPtr)
managedCloneOptions opts = managed (withCloneOptions opts)

withClonedRepository : (url : String)
                    -> (localPath : String)
                    -> (options : AnyPtr)
                    -> (GitResult GitRepository -> IO a)
                    -> IO a
withClonedRepository url localPath options act = do
  cresult <- primIO (prim_git_clone_repository url localPath options)
  repoResult <- getGitResult cresult
  result <- act (MkGitRepository <$> repoResult)
  case repoResult of
    Right ptr => pure result <* primIO (prim_git_repository_free ptr)
    _ => pure result

export
managedClonedRepository : (opts : CloneOpts)
                       -> (url : String)
                       -> (localPath : String)
                       -> Managed (GitResult GitRepository)
managedClonedRepository opts url localPath = do
  Right options <- managedCloneOptions opts
    | Left res => pure (Left res)
  managed (withClonedRepository url localPath options)
