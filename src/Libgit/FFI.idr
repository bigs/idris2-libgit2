module Libgit.FFI

import Prelude
import Control.Monad.State

public export
libgit : String -> String
libgit fn = "C:" ++ fn ++ ",libgit2"

public export
libgitWrapper : String -> String
libgitWrapper fn = "C:" ++ fn ++ ",libgit_idris_wrapper"

%foreign (libgit "git_libgit2_init")
libgit_init : PrimIO Int

%foreign (libgit "git_libgit2_shutdown")
libgit_shutdown : PrimIO Int

export
data GitContext = MkGitContext

initGitContext : IO (Either Int GitContext)
initGitContext = do
  res <- primIO $ libgit_init
  if res >= 0
    then pure $ Right MkGitContext
    else pure $ Left res

shutdownGitContext : GitContext -> IO Int
shutdownGitContext _ = primIO libgit_shutdown

implementation Foldable (Either l) where
  foldr f m (Left l) = m
  foldr f m (Right x) = f x m

implementation Traversable (Either l) where
  traverse _ (Left x) = pure $ Left x
  traverse f (Right x) = Right <$> f x

public export
GitT : (m: Type -> Type) -> (ty: Type) -> Type
GitT = StateT GitContext

export
runGitT : HasIO m => GitT m a -> m (Either Int a)
runGitT action = do
  eCtx <- liftIO initGitContext
  for eCtx $ \ctx => do
    (res, ctx') <- runStateT action ctx
    _ <- liftIO . shutdownGitContext $ ctx'
    pure res
