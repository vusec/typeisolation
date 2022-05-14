source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# safestack
cflags="$cflags -mretpoline"
ldflagsalways="$ldflagsalways -mretpoline"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
