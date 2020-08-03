module Libgit.Clone

import Prelude
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

GitCloneOptions : Type
GitCloneOptions = Struct "git_clone_options" []

GitRepository : Type
GitRepository = Struct "git_repository" []

%foreign (libgitWrapper "mk_clone_options")
init_clone_options : PrimIO (Ptr GitCloneOptions)

%foreign (libgit "GIT_CLONE_OPTIONS_VERSION")
GIT_CLONE_OPTIONS_VERSION : Int

%foreign (libgit "git_clone_init_options")
git_clone_init_options : Ptr GitCloneOptions -> Int -> PrimIO Int

initGitCloneOptions : GitContext -> IO (Maybe (Ptr GitCloneOptions))
initGitCloneOptions _ = do
  cloneOptions <- primIO init_clone_options
  res <- primIO $ git_clone_init_options cloneOptions GIT_CLONE_OPTIONS_VERSION
  case res of
    0 => pure $ Just cloneOptions
    _ => pure $ Nothing

%foreign (libgitWrapper "mk_git_repository")
mk_null_git_repository : Ptr (Ptr GitRepository)

%foreign (libgit "git_clone")
prim_clone : (Ptr (Ptr GitRepository)) -> String -> String -> Ptr GitCloneOptions -> PrimIO Int

export
clone : GitContext -> String -> String -> IO (Ptr (Ptr GitRepository))
clone _ url localPath = do
  let repo = mk_null_git_repository
  options <- primIO $ init_clone_options
  res <- primIO $ prim_clone repo url localPath options
  putStrLn $ "Result was: " ++ show res
  pure repo
