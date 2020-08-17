module Libgit.Git

import Libgit.FFI

import Control.Monad.Reader

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

public export
implementation Foldable (Either l) where
  foldr f m (Left l) = m
  foldr f m (Right x) = f x m

public export
implementation Traversable (Either l) where
  traverse _ (Left x) = pure $ Left x
  traverse f (Right x) = Right <$> f x

public export
GitT : (m: Type -> Type) -> (ty: Type) -> Type
GitT m a = ReaderT GitContext m a

export
runGitT : HasIO m => GitT m a -> m (Either Int a)
runGitT action = do
  eCtx <- liftIO initGitContext
  for eCtx $ \ctx => do
    res <- runReaderT action ctx
    _ <- liftIO $ shutdownGitContext ctx
    pure res
