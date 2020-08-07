module Libgit.Clone

import Prelude
import Control.Monad.Reader
import Control.Monad.Trans
import System.FFI

import Libgit.FFI

-- GitCloneOptions : Type
-- GitCloneOptions = Struct "git_clone_options" [
--   ("version", Int),
--   ("checkout_opts", Int), -- git_checkout_options
--   ("fetch_opts", Int), -- git_fetch_options
--   ("bare", Int),
--   ("local", Int), -- git_clone_local_t
--   ("checkout_branch", String),
--   ("repository_cb", Int), -- git_repository_create_cb
--   ("repository_cb_payload", Int), -- void * arbitrary payload
--   ("remote_cb", Int), -- git_remote_create_cb
--   ("remote_cb_payload", Int) -- void * arbitrary payload
-- ]

AbstractStruct : String -> Type
AbstractStruct name = Struct name []

CGitCloneOptions : Type
CGitCloneOptions = AbstractStruct "git_clone_options"

CGitRepository : Type
CGitRepository = AbstractStruct "git_repository"

export
data GitRepository = MkGitRepository (Ptr CGitRepository)

%foreign (libgitWrapper "mk_clone_options")
init_clone_options : PrimIO (Ptr CGitCloneOptions)

%foreign (libgit "GIT_CLONE_OPTIONS_VERSION")
GIT_CLONE_OPTIONS_VERSION : Int

%foreign (libgit "git_clone_init_options")
git_clone_init_options : Ptr CGitCloneOptions -> Int -> PrimIO Int

liftPrimIO : (HasIO m) => PrimIO a -> m a
liftPrimIO action = liftIO . primIO $ action

initGitCloneOptions : HasIO m => GitT m (Either Int (Ptr CGitCloneOptions))
initGitCloneOptions = do
  cloneOptions <- liftPrimIO $ init_clone_options
  res <- liftPrimIO $ git_clone_init_options cloneOptions 1
  case res of
    0 => pure $ Right cloneOptions
    _ => pure $ Left res

%foreign (libgitWrapper "mk_git_repository")
mk_null_git_repository : PrimIO (Ptr (Ptr CGitRepository))

%foreign (libgit "git_clone")
prim_clone : (Ptr (Ptr CGitRepository)) -> String -> String -> Ptr CGitCloneOptions -> PrimIO Int

%foreign (libgitWrapper "get_git_repository")
prim_get_git_repository : (Ptr (Ptr CGitRepository)) -> PrimIO (Ptr CGitRepository)

export
clone : (HasIO m, Applicative m) => String -> String -> GitT m (Either Int GitRepository)
clone url localPath = do
  repo <- liftPrimIO mk_null_git_repository
  eOptions <- initGitCloneOptions
  map join $ for eOptions $ \options => do
    res <- liftPrimIO $ prim_clone repo url localPath options
    ptr <- liftPrimIO $ prim_get_git_repository repo
    if res < 0
      then pure $ Left res
      else pure . Right $ MkGitRepository ptr

export
testClone : String -> String -> IO ()
testClone url localPath = do
  eRes <- runGitT $ do
   eRepo <- clone url localPath
   case eRepo of
     Left res => putStrLn $ "Got: " ++ show res
     Right _ => putStrLn "cloned it"
  putStrLn "Done."