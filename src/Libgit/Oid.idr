module Libgit.Oid

import Libgit.FFI
import Libgit.Git
import Libgit.Types

export
gitOidFromString : (HasIO m, Monad m)
                => String
                -> GitT i m (GitResult (GitOid i))
gitOidFromString str = do
  result <- liftIO (gitResult "git oid" (git_oid_from_string str))
  pure (MkGitOid <$> result)

export
gitOidToString : Applicative m => GitOid i -> GitT i m String
gitOidToString (MkGitOid oid) = pure (git_oid_to_string oid)
