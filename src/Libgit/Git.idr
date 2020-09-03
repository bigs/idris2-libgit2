module Libgit.Git

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import System.FFI

import Libgit.FFI
import Libgit.Types

shutdownGitContext : AnyPtr -> IO ()
shutdownGitContext _ = do
  putStrLn "free gitcontext"
  primIO prim_libgit_shutdown
  pure ()

initGitContext : forall i. IO (Either Int (GitContext i))
initGitContext = do
  res <- primIO $ prim_libgit_init
  case res >= 0 of
    True => do let ctxPtr : AnyPtr = believe_me ()
               ctxPtrManaged <- onCollectAny ctxPtr shutdownGitContext
               pure (Right (MkGitContext ctxPtrManaged))
    False => pure (Left res)

||| A Git transformer, indexed by some arbitrary `i`. The GitT transformer is
||| simply a ReaderT whose contents are opaque to end users.
export
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

||| Runs Git actions within an initialized `GitContext`.
|||
||| Returns on failure an `Int` representing the Git error code returend When
||| attempting to instantiate the libgit2 library.
||| Returns on success the result of the action.
|||
||| @action A GitT action to execute within an initialized Git context.
export
runGitT : HasIO m => (forall i. GitT i m a) -> m (Either Int a)
runGitT action = do
  let i = ()
      MkGitT readerT = action {i}
  eCtx <- liftIO $ initGitContext {i}
  traverse (runReaderT readerT) eCtx

export
toGit : Monad m => a -> GitT i m (Git i a)
toGit x = do
  ctx <- MkGitT ask
  pure (MkGit ctx x)

export
fromGit : Git i a -> a
fromGit (MkGit _ x) = x

export
gitError : Applicative m => Int -> GitT i m (GitResult a)
gitError = pure . Left

export
gitSuccess : Applicative m => (x : a) -> GitT i m (GitResult a)
gitSuccess = pure . Right
