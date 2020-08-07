module Control.Monad.Reader

public export
record ReaderT (stateType : Type) (m: Type -> Type) (a: Type) where
  constructor MkReaderT
  runReaderT : stateType -> m a

public export
implementation Functor f => Functor (ReaderT stateType f) where
  map f (MkReaderT g) = MkReaderT (\st => map f (g st))

public export
implementation Applicative f => Applicative (ReaderT stateType f) where
  pure x = MkReaderT (\st => pure x)

  (MkReaderT f) <*> (MkReaderT a) =
    MkReaderT (\st =>
      let f' = f st in
      let a' = a st in
      f' <*> a')

public export
implementation Monad m => Monad (ReaderT stateType m) where
  (MkReaderT f) >>= k =
    MkReaderT (\st => do v <- f st
                         let MkReaderT kv = k v
                         kv st)

public export
implementation HasIO m => HasIO (ReaderT stateType m) where
  liftIO f = MkReaderT (\_ => liftIO f)