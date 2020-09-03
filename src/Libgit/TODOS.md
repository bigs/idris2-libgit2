# TODOS

have GitContext wrap an AnyPtr (which in practice is just null) and give that
pointer a finalizer that calls libgit shutdown. this should guarantee corect
ordering when freeing and prevent frees before shutdown

add helper method to get the ptr out of reader and store it on each sub object
to force ordering of GC
