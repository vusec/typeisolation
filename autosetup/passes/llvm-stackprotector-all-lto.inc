source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# safestack
cflags="$cflags -fstack-protector-all"
ldflagsalways="$ldflagsalways -fstack-protector-all"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
