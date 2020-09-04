module Libgit.Types

import System.FFI

||| A simple type alias for Git results which can either return a non-zero
||| integer error code or a result.
public export
GitResult : Type -> Type
GitResult a = Either Int a

export
toGitResult : Int -> a -> GitResult a
toGitResult err x = case err < 0 of
  True => Left err
  False => Right x

||| An opaque type representing a Git repository.
public export
data GitRepository : Type where
  MkGitRepository : AnyPtr -> GitRepository

||| An opaque type representing a Git object id.
public export
data GitOid : Type where
  MkGitOid : AnyPtr -> GitOid
