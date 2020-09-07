module Libgit.Object

import Control.Monad.Managed

import Libgit.FFI
import Libgit.Types

withGitObject : GitRepository
             -> GitOid
             -> Int
             -> (GitResult GitObject -> IO a)
             -> IO a
withGitObject (MkGitRepository repoPtr) (MkGitOid oidPtr) typ act = do
  let cgr = git_lookup_object repoPtr oidPtr typ
  result <- map MkGitObject <$> getGitResult cgr
  actResult <- act result
  case result of
    Right (MkGitObject ptr) => pure actResult <* primIO (prim_git_object_free ptr)
    Left _ => pure actResult

export
managedGitObject : GitRepository
                -> GitOid
                -> Int
                -> Managed (GitResult GitObject)
managedGitObject repo oid typ = managed (withGitObject repo oid typ)
