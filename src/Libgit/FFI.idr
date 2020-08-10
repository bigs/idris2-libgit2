module Libgit.FFI

import Prelude
import System.FFI

-- Structs

public export
AbstractStruct : String -> Type
AbstractStruct name = Struct name []

export
CGitCloneOptions : Type
CGitCloneOptions = AbstractStruct "git_clone_options"

export
CGitRepository : Type
CGitRepository = AbstractStruct "git_repository"

-- FFI string builders

public export
libgit : String -> String
libgit fn = "C:" ++ fn ++ ",libgit2"

public export
libgitWrapper : String -> String
libgitWrapper fn = "C:" ++ fn ++ ",libgit_idris_wrapper"

-- FFI functions

export
%foreign (libgit "git_libgit2_init")
libgit_init : PrimIO Int

export
%foreign (libgit "git_libgit2_shutdown")
libgit_shutdown : PrimIO Int

export
%foreign (libgitWrapper "mk_clone_options")
init_clone_options : PrimIO (Ptr CGitCloneOptions)

export
%foreign (libgit "GIT_CLONE_OPTIONS_VERSION")
GIT_CLONE_OPTIONS_VERSION : Int

export
%foreign (libgit "git_clone_init_options")
git_clone_init_options : Ptr CGitCloneOptions -> Int -> PrimIO Int

export
%foreign (libgitWrapper "mk_git_repository")
mk_null_git_repository : PrimIO (Ptr (Ptr CGitRepository))

export
%foreign (libgit "git_clone")
prim_clone : (Ptr (Ptr CGitRepository)) -> String -> String -> Ptr CGitCloneOptions -> PrimIO Int

export
%foreign (libgitWrapper "get_git_repository")
prim_get_git_repository : (Ptr (Ptr CGitRepository)) -> PrimIO (Ptr CGitRepository)
