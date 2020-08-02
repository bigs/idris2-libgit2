module Libgit.Clone

import System.FFI

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

libgit : String -> String
libgit fn = "C:" ++ fn ++ ",libgit2"

-- %foreign (libgit "git_clone")
-- prim_clone : (Ptr (Ptr GitRepository)) -> String -> String -> Ptr GitCloneOptions -> PrimIO Int
--
-- clone : String -> String -> PrimIO (Ptr GitRepository)
-- clone url localPath = do
--   let repo : Ptr (Ptr GitRepository) = 0
--   res <- prim_clone repo url localPath
--   pure repo

libgitWrapper : String -> String
libgitWrapper fn = "C:" ++ fn ++ ",libgit_idris_wrapper"

%foreign (libgitWrapper "log_something")
log_something : String -> PrimIO ()

export
logSomething : String -> IO ()
logSomething = primIO . log_something
