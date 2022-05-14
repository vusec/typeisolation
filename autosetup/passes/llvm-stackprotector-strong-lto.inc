source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# safestack
cflags="$cflags -fstack-protector-strong"
ldflagsalways="$ldflagsalways -fstack-protector-strong"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
