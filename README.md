# libgit-idris
libgit bindings for Idris 2

## Dependencies

**C Libraries**
- libgit (incl. headers)

**Idris libraries**
- [streaming](https://github.com/MarcelineVQ/idris2-streaming) (for
  Control.Monad.Managed)

## Implementation Details
The library presents a minimal API for interacting with Git repositories. The
Libgit.Types module defines wrapper types for libgit2 objects. Any libgit2
operations requiring memory management will return `Managed` references to the
types defined in Libgit.Types. Any monad transformer stack with IO should be
able to comfortably impelment `MonadManaged` and make use of these functions.

Libgit functions **must** be executed within an intialized Git context. This
context is accessible via `withGit` as defined in Libgit.Git. `withGit` is only
constrained via `HasIO` so as to permit the widest possible range of uses. Any
calls to Libgit functions made outside of a `withGit` callback will have
undefined behavior and will likely fail.

## Documentation / Usage
Effort has been made to write thorough documentation for the public-facing API.
Examples can be found in [Libgit.Examples](src/Libgit/Examples.idr).

## License
See [LICENSE](LICENSE).
