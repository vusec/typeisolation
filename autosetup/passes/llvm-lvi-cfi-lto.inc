source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# safestack
cflags="$cflags -mlvi-cfi"
ldflagsalways="$ldflagsalways -mlvi-cfi"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
