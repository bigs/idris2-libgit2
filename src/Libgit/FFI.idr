module Libgit.FFI

import Prelude
import System.FFI

-- FFI string builders

public export
libgit : String -> String
libgit fn = "C:" ++ fn ++ ",libgit2"

public export
libgitWrapper : String -> String
libgitWrapper fn = "C:" ++ fn ++ ",libgit_idris_wrapper"

-- System info helpers

export
%foreign (libgitWrapper "int_size_bytes")
prim_int_size_bytes : Int

export
%foreign (libgitWrapper "size_t_size_bytes")
prim_size_t_size_bytes : Int

-- I don't love this, but without providers I don't have a mechanism to fail at
-- compile time, so I'll default to Int which doesn't seem so harmful. Famous
-- last words?
byteLengthToType : Int -> Type
byteLengthToType len = case len of
  1 => Bits8
  2 => Bits16
  4 => Bits32
  8 => Bits64
  _ => Int

CUInt : Type
CUInt = Bits32

CSizeT : Type
CSizeT = Bits32

-- FFI structs

public export
AbstractStruct : String -> Type
AbstractStruct name = Struct name []

export
CGitRepository : Type
CGitRepository = AbstractStruct "git_repository"

export
CGitRemote : Type
CGitRemote = AbstractStruct "git_remote"

export
CGitRepositoryCreateCb : Type -> Type
CGitRepositoryCreateCb payload =
     Ptr (Ptr CGitRepository)
  -> (path : String)
  -> (bare : Int)
  -> (payload : payload)
  -> Int

-- CGitCheckoutNotifyCb : Type -> Type

-- CGitCheckoutProgressCb : Type -> Type

CGitStrArray : Type
CGitStrArray = Struct "git_strarray" [
  ("strings", Ptr String),
  ("count", CSizeT)
]

CGitTree : Type
CGitTree = AbstractStruct "git_tree"

CGitCheckoutPerfData : Type
CGitCheckoutPerfData = Struct "git_checkout_perfdata" [
  ("mkdir_calls", CSizeT),
  ("stat_calls", CSizeT),
  ("chmod_calls", CSizeT)
]

CGitCheckoutPerfDataCb : Type -> Type
CGitCheckoutPerfDataCb payload =
     Ptr CGitCheckoutPerfData
  -> payload
  -> PrimIO ()

CGitCheckoutOptions : Type -> Type -> Type -> Type
CGitCheckoutOptions notifyPayload progressPayload perfdataPayload =
  Struct "git_checkout_options" [
    ("version", CUInt),
    ("checkout_strategy", CUInt),
    ("disable_filters", Int),
    ("dir_mode", CUInt),
    ("file_mode", CUInt),
    ("file_open_flags", Int),
    -- ("notify_flags", CUInt),
    -- ("notify_cb", CGitCheckoutNotifyCb notifyPayload),
    -- ("notify_payload", Ptr notifyPayload),
    -- ("progress_cb", CGitCheckoutProgressCb progressPayload),
    -- ("progress_payload", Ptr progressPayload),
    ("paths", CGitStrArray),
    ("baseline", CGitTree),
    ("baseline_index", CGitTree),
    ("target_directory", Ptr String),
    ("ancestor_label", Ptr String),
    ("our_label", Ptr String),
    ("their_label", Ptr String)
    -- ("perfdata_cb", CGitCheckoutPerfDataCb perfdataPayload),
    -- ("perfdata_payload", perfdataPayload)
  ]

-- TODO: Define this properly
CGitRemoteCallbacks : Type
CGitRemoteCallbacks = AbstractStruct "git_remote_callbacks"

-- TODO: Define this properly
CGitProxyOptions : Type
CGitProxyOptions = AbstractStruct "git_proxy_options"

CGitFetchOptions : Type
CGitFetchOptions = Struct "git_fetch_options" [
  ("version", Int),
  ("callbacks", CGitRemoteCallbacks),
  ("prune", Int), -- enum
  ("update_fetchhead", Int),
  ("download_tags", Int), -- enum
  ("proxy_opts", CGitProxyOptions),
  ("custom_headers", CGitStrArray)
]

CGitRemoteCreateCb : Type -> Type
CGitRemoteCreateCb payload =
     Ptr (Ptr CGitRemote)
  -> Ptr (CGitRepository)
  -> (name : String)
  -> (url : String)
  -> (payload : payload)

--CGitCloneOptions : Type -> Type -> Type -> Type -> Type -> Type
--CGitCloneOptions repoPayload remotePayload notifyPayload progressPayload perfdataPayload =
export
CGitCloneOptions : Type
CGitCloneOptions =
  Struct "git_clone_options" [
    ("version", Int),
    ("checkout_opts", CGitCheckoutOptions AnyPtr AnyPtr AnyPtr),
    ("fetch_opts", CGitFetchOptions),
    ("bare", Int),
    ("local", Int),
    ("checkout_branch", Ptr String)
    -- ("repository_cb", CGitRepositoryCreateCb AnyPtr),
    -- ("repository_cb_payload", AnyPtr),
    -- ("remote_cb", CGitRemoteCreateCb AnyPtr),
    -- ("remote_cb_payload", AnyPtr)
  ]

-- FFI functions

export
%foreign (libgit "git_libgit2_init")
prim_libgit_init : PrimIO Int

export
%foreign (libgit "git_libgit2_shutdown")
prim_libgit_shutdown : PrimIO Int

export
%foreign (libgitWrapper "make_clone_options")
prim_init_clone_options : PrimIO CGitCloneOptions

export
%foreign (libgitWrapper "git_clone_options_version")
git_clone_options_version : Int

export
%foreign (libgit "git_clone_init_options")
prim_git_clone_init_options : CGitCloneOptions -> Int -> PrimIO Int

export
%foreign (libgitWrapper "make_git_repository")
prim_mk_null_git_repository : PrimIO (Ptr (Ptr CGitRepository))

export
%foreign (libgit "git_clone")
prim_clone : (Ptr (Ptr CGitRepository)) -> String -> String -> CGitCloneOptions -> PrimIO Int

export
%foreign (libgitWrapper "get_git_repository")
prim_get_git_repository : (Ptr (Ptr CGitRepository)) -> CGitRepository

export
liftPIO : (HasIO m) => PrimIO a -> m a
liftPIO action = liftIO $ primIO action
