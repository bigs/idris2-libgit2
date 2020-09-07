module Libgit.Object

import Control.Monad.Managed

import Libgit.FFI
import Libgit.Oid
import Libgit.Types

gitObject : AnyPtr -> (typ ** GitObject typ)
gitObject ptr =
  let objTyp = gitObjectTypeFromInt (git_object_type ptr) in
    (objTyp ** MkGitObject ptr)

withGitObject : GitRepository
             -> GitOid
             -> (GitResult (typ ** GitObject typ) -> IO a)
             -> IO a
withGitObject (MkGitRepository repoPtr) (MkGitOid oidPtr) act = do
  let cgr = git_lookup_object repoPtr oidPtr (gitObjectTypeToInt GitObjectAny)
  result <- getGitResult cgr
  let objResult = gitObject <$> result
  actResult <- act objResult
  case result of
    Right ptr => pure actResult <* primIO (prim_git_object_free ptr)
    Left _ => pure actResult

export
managedGitObject : GitRepository
                -> GitOid
                -> Managed (GitResult (typ ** GitObject typ))
managedGitObject repo oid = managed (withGitObject repo oid)

export
managedGitObjectFromString : GitRepository
                          -> String
                          -> Managed (GitResult (typ ** GitObject typ))
managedGitObjectFromString repo str = do
  Right oid <- managedOidFromString str
    | Left err => pure (Left err)
  managedGitObject repo oid

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

export
managedTypedGitObject : GitRepository
                     -> GitOid
                     -> (typ : GitObjectType)
                     -> Managed (GitResult (GitObject typ))
managedTypedGitObject repo oid typ = managed (withTypedGitObject repo oid typ)

export
managedTypedGitObjectFromString : GitRepository
                               -> String
                               -> (typ : GitObjectType)
                               -> Managed (GitResult (GitObject typ))
managedTypedGitObjectFromString repo str typ = do
  Right oid <- managedOidFromString str
    | Left err => pure (Left err)
  managed (withTypedGitObject repo oid typ)