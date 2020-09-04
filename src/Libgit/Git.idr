module Libgit.Git

import Control.Monad.Reader
import Control.Monad.Managed
import Control.Monad.State
import Control.Monad.Trans
import System.FFI

import Libgit.FFI
import Libgit.Types

||| A Git transformer, indexed by some arbitrary `i`. The GitT transformer is
||| simply an identity monad that indexes an underlying monad stack. This index
||| serves as a control region, limiting objects tagged with it from being used
||| outside of this context.
public export
data GitT : (i : Type) -> (m : Type -> Type) -> (a : Type) -> Type where
  MkGitT : (1 _ : m a) -> GitT i m a

unGitT : GitT i m a -> m a
unGitT (MkGitT x) = x

public export
implementation Functor f => Functor (GitT i f) where
  map f (MkGitT x) = MkGitT (f <$> x)

public export
implementation Applicative m => Applicative (GitT i m) where
  pure x = MkGitT (pure x)
  MkGitT f <*> MkGitT x = MkGitT (f <*> x)

public export
implementation Monad m => Monad (GitT i m) where
  MkGitT x >>= f = MkGitT $ x >>= unGitT . f

public export
implementation MonadTrans (GitT i) where
  lift = MkGitT

public export
implementation HasIO m => HasIO (GitT i m) where
  liftIO action = MkGitT (liftIO action)

public export
implementation MonadManaged m => MonadManaged (GitT i m) where
  use x = lift (use x)

withGit : HasIO io => io b -> io (Either Int b)
withGit act = do
  err <- liftIO (primIO prim_libgit_init)
  case err < 0 of
    True => pure (Left err)
    False => do
      res <- act
      liftIO (primIO prim_libgit_shutdown)
      pure (Right res)

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
      MkGitT maction = action {i}
  res <- liftIO (primIO prim_libgit_init)
  case res < 0 of
    True => pure (Left res)
    False => maction >>= pure . Right
