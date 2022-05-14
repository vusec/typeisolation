source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# safestack
cflags="$cflags -mretpoline -mlvi-cfi"
ldflagsalways="$ldflagsalways -mretpoline -mlvi-cfi"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
