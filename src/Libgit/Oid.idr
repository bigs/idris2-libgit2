module Libgit.Oid

import Control.Monad.Managed

import Libgit.FFI
import Libgit.Git
import Libgit.Types

withOidFromString : String -> (GitResult GitOid -> IO a) -> IO a
withOidFromString str act = do
  let cresult = git_oid_from_string str
  (err, ptr) <- getGitResultPair cresult
  res <- act (MkGitOid <$> toGitResult err ptr)
  primIO (prim_free ptr)
  pure res

export
oidFromString : String -> Managed (GitResult GitOid)
oidFromString str = managed (withOidFromString str)

export
gitOidToString : GitOid -> String
gitOidToString (MkGitOid oid) = git_oid_to_string oid
