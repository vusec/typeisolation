These are some very hacky bits which were used to build using musl/libc++/etc.

musl-weaksyms: Remove some weak symbols from musl.
libcxx-remove-sizeof: Remove some unnecessary uses of sizeof which end up being impossible to revert when TAT's LLVM IR pass runs, due to optimizations in the meantime making the size non-constant. (See also the type blacklist in the code.)
gperftools: Hacked makefiles which don't build tests (because they don't link). I did this late at night when sleep deprived, you can tell.

See also some similar bits in ../patches:

musl-symbol-hack.diff: An alternative way to force a symbol gets included, I guess.
tcmalloc-sbrk-musl.patch: Don't use __sbrk which is glibc-specific (latest upstream has a better fix for this).
