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

export
gitObject : GitRepository
         -> GitOid
         -> Managed (GitResult (typ ** GitObject typ))
gitObject repo oid = managed (withGitObject repo oid)

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

export
typedGitObject : GitRepository
              -> GitOid
              -> (typ : GitObjectType)
              -> Managed (GitResult (GitObject typ))
typedGitObject repo oid typ = managed (withTypedGitObject repo oid typ)

export
typedGitObjectFromString : GitRepository
                        -> String
                        -> (typ : GitObjectType)
                        -> Managed (GitResult (GitObject typ))
typedGitObjectFromString repo str typ = do
  Right oid <- oidFromString str
    | Left err => pure (Left err)
  managed (withTypedGitObject repo oid typ)
