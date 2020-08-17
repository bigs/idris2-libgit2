module Libgit.Git

import Control.Monad.Reader

import Libgit.FFI

||| An abstract token representing the initialized libgit2 state. Its
||| constructor is purposefully not exported, a context can only be created
||| via `runGitT`.
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

||| Runs Git actions within an initialized `GitContext`
export
runGitT : HasIO m => GitT m a -> m (Either Int a)
runGitT action = do
  eCtx <- liftIO initGitContext
  for eCtx $ \ctx => do
    res <- runReaderT action ctx
    _ <- liftIO $ shutdownGitContext ctx
    pure res
