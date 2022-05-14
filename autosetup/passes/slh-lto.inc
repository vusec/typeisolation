source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

cflags="$cflags -mllvm -x86-speculative-load-hardening"
ldflagsalways="$ldflagsalways -mllvm -x86-speculative-load-hardening"
add_lto_args_nolib -x86-speculative-load-hardening

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
