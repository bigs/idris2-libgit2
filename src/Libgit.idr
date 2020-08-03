module Libgit

import Prelude
import System.FFI

import Libgit.Clone
import Libgit.FFI

%foreign (libgit "git_libgit2_init")
libgit_init : PrimIO ()

