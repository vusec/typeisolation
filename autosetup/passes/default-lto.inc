source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"

# tcmalloc settings
CONFIG_MALLOC=default



cflags="$cflags -O3"
ldflagsalways="$ldflagsalways -O3"
