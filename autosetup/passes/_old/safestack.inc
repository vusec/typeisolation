source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# safestack
cflags="$cflags -fsanitize=safe-stack"
ldflagsalways="$ldflagsalways -fsanitize=safe-stack"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
