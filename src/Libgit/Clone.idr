module Libgit.Clone

import Prelude
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

import Libgit.FFI
import Libgit.Git

export
data GitRepository = MkGitRepository (Ptr CGitRepository)

liftPrimIO : (HasIO m) => PrimIO a -> m a
liftPrimIO action = liftIO $ primIO action

initGitCloneOptions : HasIO m => GitT i m (Either Int (Ptr CGitCloneOptions))
initGitCloneOptions = do
  cloneOptions <- liftPrimIO $ init_clone_options
  res <- liftPrimIO $ git_clone_init_options cloneOptions git_clone_options_version
  case res of
    0 => pure $ Right cloneOptions
    _ => pure $ Left res

export
clone : HasIO m => String -> String -> GitT i m (Either Int GitRepository)
clone url localPath = do
  repo <- liftPrimIO mk_null_git_repository
  eOptions <- initGitCloneOptions {i}
  map join $ for eOptions $ \options => do
    res <- liftPrimIO $ prim_clone repo url localPath options
    ptr <- liftPrimIO $ prim_get_git_repository repo
    if res < 0
      then pure $ Left res
      else pure . Right $ MkGitRepository ptr

export
testClone : String -> String -> IO ()
testClone url localPath = do
  eRes <- runGitT $ clone url localPath
  case eRes of
    Left res => putStrLn $ "Error: " ++ show res
    Right _ => putStrLn "Cloned repository"
