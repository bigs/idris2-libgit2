module Libgit.Clone

import Prelude
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Syntax
import Control.Monad.Trans
import System.FFI

import Libgit.FFI
import Libgit.Git

export
data GitRepository : (i : Type) -> Type where
  MkGitRepository : AnyPtr -> GitRepository i

public export
record CloneOpts where
  constructor MkCloneOpts
  bare : Bool
  checkoutBranch : String

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

export
clone : HasIO m
     => CloneOpts
     -> String
     -> String
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
