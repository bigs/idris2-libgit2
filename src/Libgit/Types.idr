module Libgit.Types

import System.FFI

||| A simple type alias for Git results which can either return a non-zero
||| integer error code or a result.
public export
GitResult : Type -> Type
GitResult a = Either Int a

||| An opaque type representing a Git repository.
public export
data GitRepository : (i : Type) -> Type where
  MkGitRepository : GCAnyPtr -> GitRepository i
