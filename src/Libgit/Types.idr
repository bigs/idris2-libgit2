module Libgit.Types

import System.FFI

import Libgit.Git

||| A simple type alias for Git results which can either return a non-zero
||| integer error code or a result.
public export
GitResult : Type -> Type
GitResult a = Either Int a

export
gitError : Applicative m => Int -> GitT i m (GitResult a)
gitError = pure . Left

export
gitSuccess : Applicative m => (x : a) -> GitT i m (GitResult a)
gitSuccess = pure . Right

||| An opaque type representing a Git repository.
public export
data GitRepository : (i : Type) -> Type where
  MkGitRepository : AnyPtr -> GitRepository i

