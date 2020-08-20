module Libgit.Clone

import Prelude
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Syntax
import Control.Monad.Trans

import Libgit.FFI
import Libgit.Git

export
data GitRepository : (i : Type) -> Type where
  MkGitRepository : CGitRepository -> GitRepository i

initGitCloneOptions : HasIO m => GitT i m (Either Int CGitCloneOptions)
initGitCloneOptions = do
  cloneOptions <- liftPIO $ prim_init_clone_options
  res <- liftPIO $ prim_git_clone_init_options cloneOptions git_clone_options_version
  case res of
    0 => pure $ Right cloneOptions
    _ => pure $ Left res

export
clone : HasIO m => String -> String -> GitT i m (Either Int (GitRepository i))
clone url localPath = do
  repo <- liftPIO prim_mk_null_git_repository
  eOptions <- initGitCloneOptions {i}
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
    eRes <- clone url localPath
    let result = case eRes of
                   Left res => "Error: " ++ show res
                   Right _ => "Cloned repository"
    liftIO $ putStrLn result
  putError result
  where
    putError : Either Int () -> IO ()
    putError (Left res) = putStrLn $ "Error in shutdown: " ++ show res
    putError (Right x) = pure x
