module Libgit.Oid

import Control.Monad.Managed

import Libgit.FFI
import Libgit.Types

withOidFromString : String -> (GitResult GitOid -> IO a) -> IO a
withOidFromString str act = do
  let cresult = git_oid_from_string str
  (err, ptr) <- getGitResultPair cresult
  res <- act (MkGitOid <$> toGitResult err ptr)
  primIO (prim_free ptr)
  pure res

||| Attempt to parse a Git Object ID from a string
|||
||| Returns on success a managed reference to an object ID
||| Returns on failure a Git error code
|||
||| @str a string representation of an object ID
export
oidFromString : (str : String) -> Managed (GitResult GitOid)
oidFromString str = managed (withOidFromString str)

||| Generate a hex-string representation of an object id
export
gitOidToString : GitOid -> String
gitOidToString (MkGitOid oid) = git_oid_to_string oid
