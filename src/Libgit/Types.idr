module Libgit.Types

import System.FFI

||| A simple type alias for Git results which can either return a non-zero
||| integer error code or a result.
public export
GitResult : Type -> Type
GitResult a = Either Int a

||| Given an error code and some object, create a GitResult.
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

||| Git object types
public export
data GitObjectType = GitObjectAny
                   | GitObjectBad
                   | GitObjectCommit
                   | GitObjectTree
                   | GitObjectBlob
                   | GitObjectTag
                   | GitObjectOfsDelta
                   | GitObjectRefDelta

public export
data IsCommitish : GitObjectType -> Type where
  IsCommitishCommit : IsCommitish GitObjectCommit
  IsCommitishTag : IsCommitish GitObjectTag

export
gitObjectTypeToInt : GitObjectType -> Int
gitObjectTypeToInt GitObjectAny = -2
gitObjectTypeToInt GitObjectBad = -1
gitObjectTypeToInt GitObjectCommit = 1
gitObjectTypeToInt GitObjectTree = 2
gitObjectTypeToInt GitObjectBlob = 3
gitObjectTypeToInt GitObjectTag = 4
gitObjectTypeToInt GitObjectOfsDelta = 6
gitObjectTypeToInt GitObjectRefDelta = 7

export
gitObjectTypeFromInt : Int -> GitObjectType
gitObjectTypeFromInt x = case x of
  1 => GitObjectCommit
  2 => GitObjectTree
  3 => GitObjectBlob
  4 => GitObjectTag
  6 => GitObjectOfsDelta
  7 => GitObjectRefDelta
  _ => if x == -2
         then GitObjectAny
         else GitObjectBad -- This is kind of a hack, but kind of not.

public export
data GitObject : (typ : GitObjectType) -> Type where
  MkGitObject : AnyPtr -> GitObject typ
