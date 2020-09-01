module Libgit.Clone

import Prelude
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Syntax
import Control.Monad.Trans
import System.FFI

import Libgit.FFI
import Libgit.Git

||| An opaque type representing a Git repository.
export
data GitRepository : (i : Type) -> Type where
  MkGitRepository : AnyPtr -> GitRepository i

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

initGitCloneOptions : HasIO m => CloneOpts -> GitT i m (Either Int AnyPtr)
initGitCloneOptions opts = do
  cloneOptions <- liftPIO $ prim_init_clone_options
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
clone : HasIO m
     => (opts : CloneOpts)
     -> (url : String)
     -> (localPath : String)
     -> GitT i m (Either Int (GitRepository i))
clone opts url localPath = do
  repo <- liftPIO prim_mk_null_git_repository
  eOptions <- initGitCloneOptions {i} opts
  map join $ for eOptions $ \options => do
    res <- liftPIO $ prim_clone repo url localPath options
    let ptr = prim_get_git_repository repo
    if res < 0
      then pure $ Left res
      else pure . Right $ MkGitRepository ptr

export
testClone : String -> String -> IO ()
testClone url localPath = do
  result <- runGitT $ do
    eRes <- clone (MkCloneOpts False "setoid") url localPath
    let result = case eRes of
                   Left res => "Error: " ++ show res
                   Right _ => "Cloned repository"
    liftIO $ putStrLn result
  putError result
  where
    putError : Either Int () -> IO ()
    putError (Left res) = putStrLn $ "Error in shutdown: " ++ show res
    putError (Right x) = pure x
