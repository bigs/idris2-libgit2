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
  MkGitRepository : CGitRepository -> GitRepository i

public export
record CloneOpts where
  constructor MkCloneOpts
  bare : Bool
  checkoutBranch : String

export
defaultOpts : CloneOpts
defaultOpts = MkCloneOpts False "master"

applyOpts : CloneOpts -> CGitCloneOptions -> IO ()
applyOpts cloneOpts cCloneOpts = do
  let bare' = cBool cloneOpts.bare
  primIO $ prim_apply_clone_options cCloneOpts cloneOpts.checkoutBranch bare'

initGitCloneOptions : HasIO m => CloneOpts -> GitT i m (Either Int CGitCloneOptions)
initGitCloneOptions opts = do
  cloneOptions <- liftPIO $ prim_init_clone_options
  res <- liftPIO $ prim_git_clone_init_options cloneOptions git_clone_options_version
  case res of
    0 => do
      liftIO $ applyOpts opts cloneOptions
      pure $ Right cloneOptions
    _ => pure $ Left res

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
    liftIO $ putStrLn "Trying it"
    let branch = clone_options_branch options
    liftIO $ putStrLn $ "Cloning " ++ branch
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
