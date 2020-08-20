module Libgit.Git

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

import Libgit.FFI

||| An abstract token representing the initialized libgit2 state. Its
||| constructor is purposefully not exported, a context can only be created
||| via `runGitT`.
export
data GitContext : (i : Type) -> Type where
  MkGitContext : GitContext i

initGitContext : forall i. IO (Either Int (GitContext i))
initGitContext = do
  res <- primIO $ prim_libgit_init
  if res >= 0
    then pure $ Right $ MkGitContext
    else pure $ Left res

shutdownGitContext : GitContext i -> IO Int
shutdownGitContext _ = primIO prim_libgit_shutdown

public export
implementation Foldable (Either l) where
  foldr f m (Left l) = m
  foldr f m (Right x) = f x m

public export
implementation Traversable (Either l) where
  traverse _ (Left x) = pure $ Left x
  traverse f (Right x) = Right <$> f x

public export
data GitT : (i : Type) -> (m : Type -> Type) -> (a : Type) -> Type where
  MkGitT : (1 _ : ReaderT (GitContext i) m a) -> GitT i m a

unGitT : GitT i m a -> ReaderT (GitContext i) m a
unGitT (MkGitT x) = x

public export
implementation Functor m => Functor (GitT i m) where
  map f (MkGitT x) = MkGitT (f <$> x)

public export
implementation Applicative m => Applicative (GitT i m) where
  pure x = MkGitT (pure x)

  MkGitT f <*> MkGitT x = MkGitT (f <*> x)

public export
implementation Monad m => Monad (GitT i m) where
  MkGitT x >>= f = MkGitT $ x >>= unGitT . f

public export
implementation MonadTrans m => MonadTrans (GitT i) where
  lift action = MkGitT $ lift action

public export
implementation HasIO m => HasIO (GitT i m) where
  liftIO action = MkGitT $ liftIO action

||| Runs Git actions within an initialized `GitContext`
export
runGitT : HasIO m => (forall i. GitT i m a) -> m (Either Int a)
runGitT action = do
  let i = ()
      MkGitT readerT = action {i}
  eCtx <- liftIO $ initGitContext {i}
  for eCtx $ \ctx => do
    (res, _) <- runReaderT readerT ctx
    _ <- liftIO $ shutdownGitContext ctx
    pure res
