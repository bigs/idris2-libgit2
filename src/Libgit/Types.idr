module Libgit.Types

import System.FFI

||| An abstract token representing the initialized libgit2 state. Its
||| constructor is purposefully not exported, a context can only be created
||| via `runGitT`.
public export
data GitContext : (i : Type) -> Type where
  MkGitContext : GCAnyPtr -> GitContext i

||| A simple type alias for Git results which can either return a non-zero
||| integer error code or a result.
public export
GitResult : Type -> Type
GitResult a = Either Int a

public export
data Git : (i : Type) -> (a : Type) -> Type where
  MkGit : (ctx : GitContext i) -> a -> Git i a

export
implementation Functor (Git i) where
  map f (MkGit ctx x) = MkGit ctx (f x)

||| An opaque type representing a Git repository.
public export
data GitRepository : (i : Type) -> Type where
  MkGitRepository : Git i GCAnyPtr -> GitRepository i

||| An opaque type representing a Git object id.
public export
data GitOid : (i : Type) -> Type where
  MkGitOid : Git i GCAnyPtr -> GitOid i
