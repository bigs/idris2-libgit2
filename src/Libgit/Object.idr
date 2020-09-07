module Libgit.Object

import Control.Monad.Managed

import Libgit.FFI
import Libgit.Oid
import Libgit.Types

enrichGitObject : AnyPtr -> (typ ** GitObject typ)
enrichGitObject ptr =
  let objTyp = gitObjectTypeFromInt (git_object_type ptr) in
    (objTyp ** MkGitObject ptr)

withGitObject : GitRepository
             -> GitOid
             -> (GitResult (typ ** GitObject typ) -> IO a)
             -> IO a
withGitObject (MkGitRepository repoPtr) (MkGitOid oidPtr) act = do
  let cgr = git_lookup_object repoPtr oidPtr (gitObjectTypeToInt GitObjectAny)
  result <- getGitResult cgr
  let objResult = enrichGitObject <$> result
  actResult <- act objResult
  case result of
    Right ptr => pure actResult <* primIO (prim_git_object_free ptr)
    Left _ => pure actResult

||| Retrieve an object of any type from a Git repository.
|||
||| Returns on success a dependent pair of the object's type and a managed
||| reference to the object.
||| Returns on failure a Git error code.
|||
||| @repo The GitRepository to retrieve the object from.
||| @oid  The object ID to retrieve from the repository.
export
gitObject : (repo : GitRepository)
         -> (oid : GitOid)
         -> Managed (GitResult (typ ** GitObject typ))
gitObject repo oid = managed (withGitObject repo oid)

||| Retrieve an object of any type from a Git repository based on the object
||| ID's string representation.
|||
||| Returns on success a dependent pair of the object's type and a managed
||| reference to the object.
||| Returns on failure a Git error code.
|||
||| @repo The GitRepository to retrieve the object from.
||| @str  The string representation of an object ID.
export
gitObjectFromString : GitRepository
                   -> String
                   -> Managed (GitResult (typ ** GitObject typ))
gitObjectFromString repo str = do
  Right oid <- oidFromString str
    | Left err => pure (Left err)
  gitObject repo oid

withTypedGitObject : GitRepository
                  -> GitOid
                  -> (typ : GitObjectType)
                  -> (GitResult (GitObject typ) -> IO a)
                  -> IO a
withTypedGitObject (MkGitRepository repoPtr) (MkGitOid oidPtr) typ act = do
  let cgr = git_lookup_object repoPtr oidPtr (gitObjectTypeToInt typ)
  result <- getGitResult cgr
  actResult <- act (MkGitObject <$> result)
  case result of
    Right ptr => pure actResult <* primIO (prim_git_object_free ptr)
    Left _ => pure actResult

||| Retrieve an object of a specific type from a Git repository.
|||
||| Returns on success a managed reference to the object.
||| Returns on failure a Git error code.
|||
||| @repo The GitRepository to retrieve the object from.
||| @oid  The object ID to retrieve from the repository.
||| @typ  The GitObjectType of the object to retrieve.
export
typedGitObject : GitRepository
              -> GitOid
              -> (typ : GitObjectType)
              -> Managed (GitResult (GitObject typ))
typedGitObject repo oid typ = managed (withTypedGitObject repo oid typ)

||| Retrieve an object of a specific type from a Git repository based on the
||| object ID's string representation.
|||
||| Returns on success a managed reference to the object.
||| Returns on failure a Git error code.
|||
||| @repo The GitRepository to retrieve the object from.
||| @str  The string representation of an object ID.
||| @typ  The GitObjectType of the object to retrieve.
export
typedGitObjectFromString : GitRepository
                        -> String
                        -> (typ : GitObjectType)
                        -> Managed (GitResult (GitObject typ))
typedGitObjectFromString repo str typ = do
  Right oid <- oidFromString str
    | Left err => pure (Left err)
  managed (withTypedGitObject repo oid typ)
