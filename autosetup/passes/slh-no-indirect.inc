source "$PATHROOT/autosetup/passes/helper/reset.inc"
#source "$PATHROOT/autosetup/passes/helper/lto.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

cflags="$cflags -mllvm -x86-speculative-load-hardening -mllvm -x86-slh-indirect=0"
ldflagsalways="$ldflagsalways -mllvm -x86-speculative-load-hardening -mllvm -x86-slh-indirect=0"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
